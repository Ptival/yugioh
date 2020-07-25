{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module YuGiOh.Operation
  ( Operation,
    addCardToHand,
    attack,
    chooseMove,
    directAttack,
    drawCard,
    endTurn,
    enter,
    getDeck,
    getHand,
    getHasDrawnCard,
    getHasNormalSummoned,
    getMainMonsterZone,
    getPhase,
    getPlayer,
    getStartingHandSize,
    getTurnNumber,
    handleOperation,
    removeCardFromHand,
    setHasAttacked,
    setHasDrawnCard,
    setHasNormalSummoned,
    shuffleDeck,
    summonMonster,
    switchPosition,
    tributeMonster,
    runOperation,
  )
where

import Control.Lens (set, view)
import Control.Monad (when)
import Data.List
import Data.List.Index (modifyAt)
import Polysemy (Member, Sem, interpret, makeSem)
import Polysemy.Embed (Embed, embed)
import Polysemy.Fail (Fail)
import Polysemy.Reader (Reader)
import Polysemy.State (State, get)
import Polysemy.Writer (Writer)
import System.Random.Shuffle (shuffleM)
import qualified YuGiOh.Card as Card
import YuGiOh.ChooseOption
import YuGiOh.Classes.Displayable
import YuGiOh.Classes.Flippable
import YuGiOh.Configuration
import YuGiOh.Duel
import YuGiOh.DuelHelpers
import YuGiOh.Fresh
import YuGiOh.GameEffects
import qualified YuGiOh.Lenses as L
import qualified YuGiOh.Log as Log
import YuGiOh.Mat
import qualified YuGiOh.Move as Move
import YuGiOh.Phase
import YuGiOh.Player as Player
import YuGiOh.Position
import qualified YuGiOh.Space as Space
import YuGiOh.Utils as Utils
import YuGiOh.Victory
import Prelude hiding (flip, log)

data Operation (m :: * -> *) a where
  AddCardToHand :: PlayerLens -> Card.Card -> Operation m ()
  ChooseMove :: forall m p. [Move.Move p] -> Operation m (Move.Move p)
  DestroyMonster :: PlayerLens -> Space.MonsterSpace -> Operation m ()
  DrawCard :: PlayerLens -> Operation m (Maybe Victory)
  EndTurn :: Operation m ()
  Enter :: Phase -> Operation m ()
  FlipMonster :: PlayerLens -> Space.MonsterSpace -> Operation m ()
  GetDeck :: PlayerLens -> Operation m Deck
  GetHand :: PlayerLens -> Operation m Hand
  GetHasDrawnCard :: PlayerLens -> Operation m Bool
  GetHasNormalSummoned :: PlayerLens -> Operation m Bool
  GetMainMonsterZone :: PlayerLens -> Operation m MainMonsterZone
  GetPhase :: Operation m Phase
  GetPlayer :: PlayerLens -> Operation m Player
  GetStartingHandSize :: Operation m Int
  GetTurnNumber :: Operation m Int
  InflictDamage :: PlayerLens -> Int -> Operation m (Maybe Victory)
  Log :: Log.Entry -> Operation m ()
  RemoveCardFromHand :: PlayerLens -> Card.Card -> Operation m ()
  SendToGraveyard :: PlayerLens -> Card.Card -> Operation m ()
  SetHasAttacked :: PlayerLens -> Space.MonsterSpace -> Operation m ()
  SetHasDrawnCard :: PlayerLens -> Bool -> Operation m ()
  SetHasNormalSummoned :: PlayerLens -> Bool -> Operation m ()
  ShuffleDeck :: PlayerLens -> Operation m ()
  SummonMonster :: PlayerLens -> Card.Card -> Position -> Operation m ()
  SwitchPosition :: PlayerLens -> Space.MonsterSpace -> Operation m ()
  TributeMonster :: PlayerLens -> Operation m ()

makeSem ''Operation

-- | Some operations may be derived from other operations
attack ::
  Member Operation e =>
  PlayerLens ->
  Space.MonsterSpace ->
  PlayerLens ->
  Space.MonsterSpace ->
  Sem e (Maybe Victory)
attack sourcePlayer sourceMonster targetPlayer targetMonster = do
  when (Space.isInFaceDownDefensePosition targetMonster) $
    YuGiOh.Operation.flipMonster targetPlayer targetMonster
  YuGiOh.Operation.log =<< Log.Attacked
    <$> getPlayer sourcePlayer
    <*^> sourceMonster
    <*> getPlayer targetPlayer
    <*^> targetMonster
  setHasAttacked sourcePlayer sourceMonster
  let sourceATK = view (Space.monsterCard . Card.attack) sourceMonster
  if Space.isInAttackPosition targetMonster
    then do
      let targetATK = view (Space.monsterCard . Card.attack) targetMonster
      case compare sourceATK targetATK of
        -- When the source's attack is greater than the target's attack, the
        -- target is destroyed, and damage is inflicted to the target player.
        GT -> do
          YuGiOh.Operation.destroyMonster targetPlayer targetMonster
          let damage = sourceATK - targetATK
          YuGiOh.Operation.inflictDamage targetPlayer damage
        -- When the source's attack is equal to the target's attack, both
        -- monsters are destroyed, but not damage is inflicted.
        EQ -> do
          YuGiOh.Operation.destroyMonster sourcePlayer sourceMonster
          YuGiOh.Operation.destroyMonster targetPlayer targetMonster
          return Nothing
        -- When the source's attack is lower than the target's attack, the
        -- source is destroyed, and damage is inflicted to the source player.
        LT -> do
          YuGiOh.Operation.destroyMonster sourcePlayer sourceMonster
          let damage = targetATK - sourceATK
          YuGiOh.Operation.inflictDamage sourcePlayer damage
    else-- target monster is in defense position
    do
      let targetDEF = view (Space.monsterCard . Card.defense) targetMonster
      case compare sourceATK targetDEF of
        -- When the source's attack is greater than the target's defense, the
        -- target is destroyed, but no damage is inflicted.
        GT -> do
          YuGiOh.Operation.destroyMonster targetPlayer targetMonster
          return Nothing
        -- When the source's attack is equal to the target's defense, no card
        -- is destroyed, and no damage is inflicted.
        EQ ->
          return Nothing
        -- When the source's attack is lower than the target's defense, no card
        -- is destroyed, but damage is inflicted to the source player.
        LT -> do
          let damage = targetDEF - sourceATK
          YuGiOh.Operation.inflictDamage sourcePlayer damage

directAttack ::
  Member Operation e =>
  PlayerLens ->
  Space.MonsterSpace ->
  PlayerLens ->
  Sem e (Maybe Victory)
directAttack player monster target = do
  YuGiOh.Operation.log
    =<< Log.DirectAttacked <$> getPlayer player <*^> monster <*> getPlayer target
  setHasAttacked player monster
  let sourceATK = view (Space.monsterCard . Card.attack) monster
  YuGiOh.Operation.inflictDamage target sourceATK

runOperation ::
  Member ChooseOption e =>
  Member (Embed IO) e =>
  Member Fail e =>
  Member Fresh e =>
  Member (Reader Configuration) e =>
  Member (State Duel) e =>
  Member (Writer Log.Log) e =>
  Operation m a ->
  Sem e a
runOperation operation = case operation of
  AddCardToHand player card -> overLensed (L.hand player) (card :)
  ChooseMove options -> do
    duel <- get
    chooseOption duel display options
  DestroyMonster player monster -> do
    Utils.log =<< Log.Destroyed <$> getLensed player <*^> monster
    overLensed (player . mat . mainMonsterZone) $ map (Space.destroyMonster monster)
    handleOperation $ YuGiOh.Operation.sendToGraveyard player (view Space.monsterCard monster)
  DrawCard player ->
    getLensed (L.deck player) >>= \case
      [] ->
        do
          winner <- getALensed =<< opponentOf player
          return $ Just (makeVictory winner OpponentRanOutOfCards)
      drawn : restOfDeck ->
        do
          handleOperation $
            do
              setHasDrawnCard player True
              addCardToHand player drawn
          setLensed (L.deck player) restOfDeck
          Utils.log =<< Log.DrewCard <$> getLensed player <*^> drawn
          return Nothing
  EndTurn ->
    do
      finishedTurnPlayer <- getLensed L.currentPlayer
      upcomingTurnPlayer <- getLensed L.otherPlayer
      setLensed L.currentPlayer $ Player.prepareForNewTurn upcomingTurnPlayer
      setLensed L.otherPlayer finishedTurnPlayer
      overLensed L.turn (+ 1)
      setLensed L.phase DrawPhase
      Utils.log =<< Log.Turn
        <$> getLensed L.turn
        <*> getLensed L.currentPlayer
        <*> getLensed L.otherPlayer
  Enter enteredPhase ->
    do
      Utils.log $ Log.Entered enteredPhase
      setLensed L.phase enteredPhase
  FlipMonster player monster ->
    do
      Utils.log =<< Log.Flipped <$> getLensed player <*^> monster
      overMonster player monster flip
  GetDeck player -> getLensed (L.deck player)
  GetHand player -> getLensed (L.hand player)
  GetHasDrawnCard player -> getLensed (L.hasDrawnCard player)
  GetHasNormalSummoned player -> getLensed (L.hasNormalSummoned player)
  GetMainMonsterZone player -> getLensed (L.mainMonsterZone player)
  GetPhase -> getLensed L.phase
  GetPlayer player -> getLensed player
  GetStartingHandSize -> askLensed startingHandSize
  GetTurnNumber -> getLensed turn
  InflictDamage player damage ->
    do
      overLensed player $ Player.inflictDamage damage
      damagedPlayer <- getLensed player
      damagedPlayerLifePoints <- getLensed (player . lifePoints)
      Utils.log $ Log.DamageInflicted damagedPlayer damage
      return $
        if damagedPlayerLifePoints == 0
          then Just $ makeVictory damagedPlayer OpponentLPReducedToZero
          else Nothing
  Log entry -> Utils.log entry
  SetHasDrawnCard player value -> setLensed (L.hasDrawnCard player) value
  SetHasNormalSummoned player value -> setLensed (L.hasNormalSummoned player) value
  ShuffleDeck player ->
    do
      currentDeck <- getLensed (L.deck player)
      shuffledDeck <- embed (shuffleM currentDeck)
      setLensed (L.deck player) shuffledDeck
  YuGiOh.Operation.SwitchPosition player monster ->
    do
      playerMainMonsterZone <- getLensed (L.mainMonsterZone player)
      let monsters = Space.filterMonsterCards playerMainMonsterZone
      case findIndex (Space.isSameMonster monster) monsters of
        Nothing -> fail "Could not find the monster whose position to switch"
        Just index ->
          do
            overLensed (L.mainMonsterZone player) $
              modifyAt index (Space.whenMonster Space.switchPosition)
            newMainMonsterZone <- getLensed (L.mainMonsterZone player)
            let newMonsterSpace = Space.filterMonsterCards newMainMonsterZone !! index
            Utils.log =<< Log.SwitchedPosition <$> getLensed player <*^> newMonsterSpace <*^> index
  RemoveCardFromHand player card -> overLensed (L.hand player) (delete card)
  SendToGraveyard player card ->
    do
      Utils.log =<< Log.SentToGraveyard <$> getLensed player <*^> card
      overLensed (player . mat . graveyard) (card :)
  SetHasAttacked player monster -> overMonster player monster (set Space.hasAttacked True)
  SummonMonster player card position ->
    do
      handleOperation $
        do
          removeCardFromHand player card
          setHasNormalSummoned player True
      monsterSpace <- Space.summonMonster card position
      currentPlayerMainMonsterZone <- getLensed (L.mainMonsterZone player)
      case findIndex Space.isEmpty currentPlayerMainMonsterZone of
        Nothing -> fail "SummonMonster: Could not find a space on the mat to summon the card"
        Just index ->
          do
            overLensed (L.mainMonsterZone L.currentPlayer)
              $ modifyAt index
              $ const
              $ Space.ScopedSpace monsterSpace
            Utils.log
              =<< Log.NormalSummoned <$> getLensed L.currentPlayer <*^> monsterSpace <*^> index
  TributeMonster player ->
    do
      playerMainMonsterZone <- getLensed (L.mainMonsterZone player)
      tribute <- chooseTribute $ Space.monsterSpaces playerMainMonsterZone
      handleOperation $ YuGiOh.Operation.destroyMonster player tribute

handleOperation ::
  Member ChooseOption e =>
  Member (Embed IO) e =>
  Member Fail e =>
  Member Fresh e =>
  Member (Reader Configuration) e =>
  Member (State Duel) e =>
  Member (Writer Log.Log) e =>
  Sem (Operation ': e) a ->
  Sem e a
handleOperation = interpret runOperation
