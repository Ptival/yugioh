{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

-- | During the battle phase, a player may choose to attack a monster.
module YuGiOh.BattlePhase
  ( battlePhase,
  )
where

import Control.Lens
import Control.Monad
import Polysemy
import qualified YuGiOh.Lenses as L
import YuGiOh.Move
import YuGiOh.Operation
import YuGiOh.Phase
import qualified YuGiOh.Space as S
import YuGiOh.Victory
import Prelude hiding (log)

validMoves ::
  Member Operation e =>
  Sem e [Move 'Battle]
validMoves = do
  currentPlayerMainMonsterZone <- getMainMonsterZone L.currentPlayer
  otherPlayerMainMonsterZone <- getMainMonsterZone L.otherPlayer
  let attackMoves =
        [ Attack sourceMonster targetMonster
          | sourceMonster <- S.monsterSpaces currentPlayerMainMonsterZone,
            S.isInAttackPosition sourceMonster,
            not $ view S.hasAttacked sourceMonster,
            targetMonster <- S.monsterSpaces otherPlayerMainMonsterZone
        ]
  let directAttackMoves =
        [ DirectAttack sourceMonster
          | length (S.monsterSpaces otherPlayerMainMonsterZone) == 0,
            sourceMonster <- S.monsterSpaces currentPlayerMainMonsterZone,
            S.isInAttackPosition sourceMonster,
            not $ view S.hasAttacked sourceMonster
        ]
  return $
    []
      ++ attackMoves
      ++ directAttackMoves
      ++ [EndBattlePhase, EndTurn]

battlePhase ::
  Member Operation e =>
  Sem e (Maybe Victory)
battlePhase = validMoves >>= chooseMove >>= \case
  Attack sourceMonster targetMonster -> do
    attack L.currentPlayer sourceMonster L.otherPlayer targetMonster
  DirectAttack monster -> do
    directAttack L.currentPlayer monster L.otherPlayer
  EndBattlePhase -> do
    enterEndPhase
    return Nothing
  EndTurn -> do
    endTurn
    return Nothing
