{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | During the battle phase, a player may choose to attack a monster.
module YuGiOh.BattlePhase
  ( battleStep,
    damageStep,
    endStep,
    startStep,
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
  Sem e [Move ('BattlePhase 'BattleStep)]
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
          | null (S.monsterSpaces otherPlayerMainMonsterZone),
            sourceMonster <- S.monsterSpaces currentPlayerMainMonsterZone,
            S.isInAttackPosition sourceMonster,
            not $ view S.hasAttacked sourceMonster
        ]
  return $
    []
      ++ attackMoves
      ++ directAttackMoves
      ++ [EndBattlePhase, EndTurn]

startStep ::
  Member Operation e =>
  Sem e (Maybe Victory)
startStep =
  do
    enter $ BattlePhase BattleStep
    return Nothing

battleStep ::
  Member Operation e =>
  Sem e (Maybe Victory)
battleStep = validMoves >>= chooseMove >>= \case
  Attack sourceMonster targetMonster ->
    attack L.currentPlayer sourceMonster L.otherPlayer targetMonster
  DirectAttack monster ->
    directAttack L.currentPlayer monster L.otherPlayer
  EndBattlePhase -> do
    enter EndPhase
    return Nothing
  EndTurn -> do
    endTurn
    return Nothing

damageStep ::
  Member Operation e =>
  Sem e (Maybe Victory)
damageStep =
  do
    enter EndPhase
    return Nothing

endStep ::
  Member Operation e =>
  Sem e (Maybe Victory)
endStep =
  do
    enter EndPhase
    return Nothing
