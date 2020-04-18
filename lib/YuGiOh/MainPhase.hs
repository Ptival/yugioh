{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | During the main phase, a player may choose to summon a monster.
module YuGiOh.MainPhase
  ( mainPhase,
  )
where

import Control.Lens (view)
import Control.Monad (replicateM_)
import Polysemy (Member, Sem)
import YuGiOh.Card
import qualified YuGiOh.Lenses as L
import YuGiOh.Move
import YuGiOh.Operation
import YuGiOh.Phase
import YuGiOh.Position as Position
import qualified YuGiOh.Space as S
import YuGiOh.Victory
import Prelude hiding (log)

tributeFor :: Int -> Int
tributeFor n
  | n >= 5 && n <= 6 = 1
  | n >= 7 && n <= 8 = 2
  | n >= 9 = 3
  | otherwise = error "Only monsters above level 4 can be tribute summoned"

validMoves ::
  Member Operation e =>
  Sem e [Move 'Main]
validMoves = do
  currentPlayerHand <- getHand L.currentPlayer
  currentPlayerHasNormalSummoned <- getHasNormalSummoned L.currentPlayer
  currentPlayerMainMonsterZone <- getMainMonsterZone L.currentPlayer
  let normalSummonMoves =
        if not currentPlayerHasNormalSummoned
          && any S.isEmpty currentPlayerMainMonsterZone
          then
            [ NormalSummon card position
              | card <- currentPlayerHand,
                view level card <= 4,
                position <- [Position.Attack, FaceDownDefense]
            ]
          else []
  let tributeSummonMoves =
        if not currentPlayerHasNormalSummoned
          then
            [ TributeSummon card position
              | card <- currentPlayerHand,
                let cardLevel = view level card,
                cardLevel > 4,
                length (S.monsterSpaces currentPlayerMainMonsterZone) >= tributeFor cardLevel,
                position <- [Position.Attack, FaceDownDefense]
            ]
          else []
  let switchPositionMoves =
        [ SwitchPosition monster
          | monster <- S.monsterSpaces currentPlayerMainMonsterZone,
            not $ view L.hasSwitchedPosition monster
        ]
  return $
    [EndMainPhase, EndTurn]
      ++ normalSummonMoves
      ++ tributeSummonMoves
      ++ switchPositionMoves

mainPhase ::
  Member Operation e =>
  Sem e (Maybe Victory)
mainPhase = validMoves >>= chooseMove >>= \case
  EndMainPhase -> do
    enterBattlePhase
    return Nothing
  EndTurn -> do
    endTurn
    return Nothing
  NormalSummon card position -> do
    summonMonster L.currentPlayer card position
    return Nothing
  TributeSummon card position -> do
    let cardLevel = view level card
    let requiredTributes = tributeFor cardLevel
    replicateM_ requiredTributes $ tributeMonster L.currentPlayer
    summonMonster L.currentPlayer card position
    return Nothing
  SwitchPosition monster -> do
    switchPosition L.currentPlayer monster
    return Nothing
