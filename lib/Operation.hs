{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |

module Operation
  ( Operations
  -- , Operation.addCardToHand
  , Operation.attack
  , Operation.chooseMove
  , Operation.directAttack
  , Operation.drawCard
  , Operation.endTurn
  , Operation.enterBattlePhase
  , Operation.enterEndPhase
  , Operation.enterMainPhase
  -- , Operation.getDeck
  , Operation.getHand
  , Operation.getHasDrawnCard
  , Operation.getHasNormalSummoned
  , Operation.getMainMonsterZone
  -- , Operation.getMonsterSpaces
  , Operation.getPhase
  -- , Operation.getPlayer
  , Operation.getStartingHandSize
  , Operation.handleOperation
  -- , Operation.removeCardFromHand
  -- , Operation.setHasAttacked
  -- , Operation.setHasDrawnCard
  -- , Operation.setHasNormalSummoned
  , Operation.shuffleDeck
  , Operation.summonMonster
  , Operation.switchPosition
  , Operation.tributeMonster
  ) where

import           Control.Eff              (Member, lift)
import           Control.Eff.Extend       (Eff(..), (^$), decomp, send)
import           Control.Eff.State.Strict (get)
import           Control.Lens             (set, view)
import           Data.List
import           Data.List.Index          (modifyAt)
import           Prelude                  hiding (log)
import           System.Random.Shuffle    (shuffleM)

import           Card
import           ChooseOption
import           Configuration
import           Duel
import           DuelHelpers
import           GameEffects
import qualified Lenses                   as L
import           Log
import           Mat
import           Move
import           Phase
import           Player
import           Position
import           Space
import           Utils
import           Victory

data Operation a where
  AddCardToHand        :: PlayerLens ->         Card ->               Operation ()
  ChooseMove           ::     forall p.     [Move p] ->               Operation (Move p)
  DestroyMonster       :: PlayerLens -> MonsterSpace ->               Operation ()
  DrawCard             :: PlayerLens ->                               Operation (Maybe Victory)
  EndTurn              ::                                             Operation ()
  EnterBattlePhase     ::                                             Operation ()
  EnterEndPhase        ::                                             Operation ()
  EnterMainPhase       ::                                             Operation ()
  FlipMonster          :: PlayerLens -> MonsterSpace ->               Operation ()
  GetDeck              :: PlayerLens ->                               Operation Deck
  GetHand              :: PlayerLens ->                               Operation Hand
  GetHasDrawnCard      :: PlayerLens ->                               Operation Bool
  GetHasNormalSummoned :: PlayerLens ->                               Operation Bool
  GetMainMonsterZone   :: PlayerLens ->                               Operation MainMonsterZone
  GetPhase             ::                                             Operation Phase
  GetPlayer            :: PlayerLens ->                               Operation Player
  GetStartingHandSize  ::                                             Operation Int
  InflictDamage        :: PlayerLens -> Int          ->               Operation (Maybe Victory)
  Log                  :: Entry      ->                               Operation ()
  RemoveCardFromHand   :: PlayerLens ->         Card ->               Operation ()
  SendToGraveyard      :: PlayerLens ->         Card ->               Operation ()
  SetHasAttacked       :: PlayerLens -> MonsterSpace ->               Operation ()
  SetHasDrawnCard      :: PlayerLens ->         Bool ->               Operation ()
  SetHasNormalSummoned :: PlayerLens ->         Bool ->               Operation ()
  ShuffleDeck          :: PlayerLens ->                               Operation ()
  SummonMonster        :: PlayerLens ->         Card ->   Position -> Operation ()
  SwitchPosition       :: PlayerLens -> MonsterSpace ->               Operation ()
  TributeMonster       :: PlayerLens ->                               Operation ()

type Operations e = Member Operation e

addCardToHand :: ( Member Operation e ) => PlayerLens -> Card -> Eff e ()
addCardToHand player card = send $ AddCardToHand player card

chooseMove :: ( Member Operation e ) => [Move p] -> Eff e (Move p)
chooseMove options = send $ ChooseMove options

destroyMonster :: ( Member Operation e ) => PlayerLens -> MonsterSpace -> Eff e ()
destroyMonster player monster = send $ Operation.DestroyMonster player monster

drawCard :: ( Member Operation e ) => PlayerLens -> Eff e (Maybe Victory)
drawCard player = send $ Operation.DrawCard player

endTurn :: ( Member Operation e ) => Eff e ()
endTurn = send $ Operation.EndTurn

enterBattlePhase :: ( Member Operation e ) => Eff e ()
enterBattlePhase = send $ EnterBattlePhase

enterEndPhase :: ( Member Operation e ) => Eff e ()
enterEndPhase = send $ EnterEndPhase

enterMainPhase :: ( Member Operation e ) => Eff e ()
enterMainPhase = send $ EnterMainPhase

flipMonster :: ( Member Operation e ) => PlayerLens -> MonsterSpace -> Eff e ()
flipMonster player monster = send $ FlipMonster player monster

-- getDeck :: ( Member Operation e ) => PlayerLens -> Eff e Deck
-- getDeck player = send $ GetDeck player

getHand :: ( Member Operation e ) => PlayerLens -> Eff e Hand
getHand player = send $ GetHand player

getHasDrawnCard :: ( Member Operation e ) => PlayerLens -> Eff e Bool
getHasDrawnCard player = send $ GetHasDrawnCard player

getHasNormalSummoned :: ( Member Operation e ) => PlayerLens -> Eff e Bool
getHasNormalSummoned player = send $ GetHasNormalSummoned player

getMainMonsterZone :: ( Member Operation e ) => PlayerLens -> Eff e MainMonsterZone
getMainMonsterZone player = send $ GetMainMonsterZone player

-- getMonsterSpaces :: ( Member Operation e ) => PlayerLens -> Eff e [MonsterSpace]
-- getMonsterSpaces player = monsterSpaces <$> getMainMonsterZone player

getPhase :: ( Member Operation e ) => Eff e Phase
getPhase = send GetPhase

getPlayer :: ( Member Operation e ) => PlayerLens -> Eff e Player
getPlayer player = send $ GetPlayer player

getStartingHandSize :: ( Member Operation e ) => Eff e Int
getStartingHandSize = send $ GetStartingHandSize

inflictDamage :: ( Member Operation e ) => PlayerLens -> Int -> Eff e (Maybe Victory)
inflictDamage player damage = send $ InflictDamage player damage

log :: ( Member Operation e ) => Entry -> Eff e ()
log entry = send $ Log entry

removeCardFromHand :: ( Member Operation e ) => PlayerLens -> Card -> Eff e ()
removeCardFromHand player card = send $ RemoveCardFromHand player card

sendToGraveyard :: ( Member Operation e ) => PlayerLens -> Card -> Eff e ()
sendToGraveyard player card = send $ SendToGraveyard player card

setHasAttacked :: ( Member Operation e ) => PlayerLens -> MonsterSpace -> Eff e ()
setHasAttacked player monster = send $ SetHasAttacked player monster

setHasDrawnCard :: ( Member Operation e ) => PlayerLens -> Bool -> Eff e ()
setHasDrawnCard player value = send $ SetHasDrawnCard player value

setHasNormalSummoned :: ( Member Operation e ) => PlayerLens -> Bool -> Eff e ()
setHasNormalSummoned player value = send $ SetHasNormalSummoned player value

shuffleDeck :: ( Member Operation e ) => PlayerLens -> Eff e ()
shuffleDeck player = send $ ShuffleDeck player

summonMonster :: ( Member Operation e ) => PlayerLens -> Card -> Position -> Eff e ()
summonMonster player card position = send $ SummonMonster player card position

switchPosition :: ( Member Operation e ) => PlayerLens -> MonsterSpace -> Eff e ()
switchPosition player monster = send $ Operation.SwitchPosition player monster

tributeMonster :: ( Member Operation e ) => PlayerLens -> Eff e ()
tributeMonster player = send $ TributeMonster player

-- | Some operations may be derived from other operations

attack ::
  Operations e =>
  PlayerLens -> MonsterSpace -> PlayerLens -> MonsterSpace -> Eff e (Maybe  Victory)
attack sourcePlayer sourceMonster targetPlayer targetMonster = do
  if isInFaceDownDefensePosition targetMonster
    then Operation.flipMonster targetPlayer targetMonster
    else return ()
  Operation.log =<< Attacked
    <$>  getPlayer sourcePlayer
    <*^> sourceMonster
    <*>  getPlayer targetPlayer
    <*^> targetMonster
  setHasAttacked sourcePlayer sourceMonster
  let sourceATK = view (monsterCard . Card.attack) sourceMonster
  if isInAttackPosition targetMonster
    then do
    let targetATK = view (monsterCard . Card.attack) targetMonster
    case compare sourceATK targetATK of
      -- When the source's attack is greater than the target's attack, the
      -- target is destroyed, and damage is inflicted to the target player.
      GT -> do
        Operation.destroyMonster targetPlayer targetMonster
        let damage = sourceATK - targetATK
        Operation.inflictDamage targetPlayer damage

      -- When the source's attack is equal to the target's attack, both
      -- monsters are destroyed, but not damage is inflicted.
      EQ -> do
        Operation.destroyMonster sourcePlayer sourceMonster
        Operation.destroyMonster targetPlayer targetMonster
        return Nothing

      -- When the source's attack is lower than the target's attack, the
      -- source is destroyed, and damage is inflicted to the source player.
      LT -> do
        Operation.destroyMonster sourcePlayer sourceMonster
        let damage = targetATK - sourceATK
        Operation.inflictDamage sourcePlayer damage

    -- target monster is in defense position
    else do
    let targetDEF = view (monsterCard . defense) targetMonster
    case compare sourceATK targetDEF of
      -- When the source's attack is greater than the target's defense, the
      -- target is destroyed, but no damage is inflicted.
      GT -> do
        Operation.destroyMonster targetPlayer targetMonster
        return Nothing

      -- When the source's attack is equal to the target's defense, no card
      -- is destroyed, and no damage is inflicted.
      EQ -> do
        return Nothing

      -- When the source's attack is lower than the target's defense, no card
      -- is destroyed, but damage is inflicted to the source player.
      LT -> do
        let damage = targetDEF - sourceATK
        Operation.inflictDamage sourcePlayer damage

directAttack ::
  Operations e =>
  PlayerLens -> MonsterSpace -> PlayerLens -> Eff e (Maybe  Victory)
directAttack player monster target = do
  Operation.log =<< DirectAttacked <$> getPlayer player <*^> monster <*> getPlayer target
  setHasAttacked player monster
  let sourceATK = view (monsterCard . Card.attack) monster
  Operation.inflictDamage target sourceATK

runOperation ::
  GameEffects e =>
  Operation a -> Eff e a
runOperation operation = case operation of

  AddCardToHand player card -> overLensed (L.hand player) (card :)

  ChooseMove options -> do
    duel <- get
    chooseOption duel Move.display options

  DestroyMonster player monster -> do
    Utils.log =<< Destroyed <$> getLensed player <*^> monster
    overLensed (player . mat . mainMonsterZone) $ map (Space.destroyMonster monster)
    handleOperation $ Operation.sendToGraveyard player (view monsterCard monster)

  Operation.DrawCard player -> do
    getLensed (L.deck player) >>= \case
      [] -> do
        winner <- getALensed =<< opponentOf player
        return $ Just (makeVictory winner OpponentRanOutOfCards)
      drawn : restOfDeck -> do
        handleOperation $ do
          setHasDrawnCard player True
          addCardToHand   player drawn
        setLensed  (L.deck player) restOfDeck
        Utils.log =<< DrewCard <$> getLensed player <*^> drawn
        return Nothing

  Operation.EndTurn -> do
    finishedTurnPlayer <- getLensed L.currentPlayer
    upcomingTurnPlayer <- getLensed L.otherPlayer
    setLensed  L.currentPlayer $ Player.prepareForNewTurn upcomingTurnPlayer
    setLensed  L.otherPlayer   finishedTurnPlayer
    overLensed L.turn          (+ 1)
    setLensed  L.phase         Draw
    Utils.log =<< Log.Turn
      <$> getLensed L.turn
      <*> getLensed L.currentPlayer
      <*> getLensed L.otherPlayer

  EnterBattlePhase -> Utils.log Log.EndMainPhase   >> setLensed L.phase Battle
  EnterEndPhase    -> Utils.log Log.EndBattlePhase >> setLensed L.phase End
  EnterMainPhase   -> Utils.log Log.EndDrawPhase   >> setLensed L.phase Main

  FlipMonster player monster -> do
    Utils.log =<< Flipped <$> getLensed player <*^> monster
    overMonster player monster Space.flip

  GetDeck              player -> getLensed (L.deck              player)
  GetHand              player -> getLensed (L.hand              player)
  GetHasDrawnCard      player -> getLensed (L.hasDrawnCard      player)
  GetHasNormalSummoned player -> getLensed (L.hasNormalSummoned player)
  GetMainMonsterZone   player -> getLensed (L.mainMonsterZone   player)
  GetPhase                    -> getLensed L.phase
  GetPlayer            player -> getLensed player
  GetStartingHandSize         -> askLensed startingHandSize

  InflictDamage player damage -> do
    overLensed player $ Player.inflictDamage damage
    damagedPlayer           <- getLensed player
    damagedPlayerLifePoints <- getLensed (player . lifePoints)
    Utils.log $ DamageInflicted damagedPlayer damage
    return $ if damagedPlayerLifePoints == 0
      then Just $ makeVictory damagedPlayer OpponentLPReducedToZero
      else Nothing

  Log entry -> Utils.log entry

  SetHasDrawnCard      player value -> setLensed (L.hasDrawnCard      player) value
  SetHasNormalSummoned player value -> setLensed (L.hasNormalSummoned player) value

  ShuffleDeck player -> do
    currentDeck  <- getLensed (L.deck player)
    shuffledDeck <- lift (shuffleM currentDeck)
    setLensed (L.deck player) shuffledDeck

  Operation.SwitchPosition player monster -> do
    playerMainMonsterZone <- getLensed (L.mainMonsterZone player)
    let monsters         = filterMonsterCards playerMainMonsterZone
    case findIndex (isSameMonster monster) monsters of
      Nothing -> fail "Could not find the monster whose position to switch"
      Just index -> do
        overLensed (L.mainMonsterZone player)
          $ modifyAt index (whenMonster Space.switchPosition)
        newMainMonsterZone <- getLensed (L.mainMonsterZone player)
        let newMonsterSpace = filterMonsterCards newMainMonsterZone !! index
        Utils.log =<< SwitchedPosition <$> getLensed player <*^> newMonsterSpace <*^> index

  RemoveCardFromHand player card -> overLensed (L.hand player) (delete card)

  SendToGraveyard player card -> do
    Utils.log =<< SentToGraveyard <$> getLensed player <*^> card
    overLensed (player . mat . graveyard) (card :)

  SetHasAttacked player monster -> overMonster player monster (set hasAttacked True)

  SummonMonster player card position -> do
    handleOperation $ do
      removeCardFromHand   player card
      setHasNormalSummoned player True
    monsterSpace                 <- Space.summonMonster card position
    currentPlayerMainMonsterZone <- getLensed (L.mainMonsterZone player)
    case findIndex isEmpty currentPlayerMainMonsterZone of
      Nothing -> fail "SummonMonster: Could not find a space on the mat to summon the card"
      Just index -> do
        overLensed (L.mainMonsterZone L.currentPlayer)
          $ modifyAt index
          $ const $ ScopedSpace monsterSpace
        Utils.log =<< NormalSummoned <$> getLensed L.currentPlayer <*^> monsterSpace <*^> index

  TributeMonster player -> do
    playerMainMonsterZone <- getLensed (L.mainMonsterZone player)
    tribute               <- chooseTribute $ monsterSpaces playerMainMonsterZone
    handleOperation $ Operation.destroyMonster player tribute

handleOperation :: ( GameEffects e ) => Eff '[Operation] a -> Eff e a
handleOperation (Val x) = return x
handleOperation (E u q) = case decomp q of
  Right operation -> do
    result <- runOperation operation
    handleOperation (u ^$ result)
  Left  _ -> error "This can not happen" -- because `Operation` is the only effect
