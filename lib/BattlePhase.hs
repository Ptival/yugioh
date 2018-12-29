{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

-- | During the battle phase, a player may choose to attack a monster.

module BattlePhase (
  battlePhase,
  ) where

import           Control.Eff   (Eff)
import           Control.Lens
import           Control.Monad

import qualified Lenses        as L
import           Move
import           Operation
import           Phase
import           Prelude       hiding (log)
import qualified Space         as S
import           Victory

validMoves ::
  Operations e =>
  Eff e [Move 'Battle]
validMoves = do
  currentPlayerMainMonsterZone <- getMainMonsterZone L.currentPlayer
  otherPlayerMainMonsterZone   <- getMainMonsterZone L.otherPlayer

  let attackMoves = [ Move.Attack sourceMonster targetMonster
                    | sourceMonster <- S.monsterSpaces currentPlayerMainMonsterZone
                    , S.isInAttackPosition sourceMonster
                    , not $ view S.hasAttacked sourceMonster
                    , targetMonster <- S.monsterSpaces otherPlayerMainMonsterZone
                    ]

  let directAttackMoves = [ Move.DirectAttack sourceMonster
                          | length (S.monsterSpaces otherPlayerMainMonsterZone) == 0
                          , sourceMonster <- S.monsterSpaces currentPlayerMainMonsterZone
                          , S.isInAttackPosition sourceMonster
                          , not $ view S.hasAttacked sourceMonster
                          ]

  return $ []
    ++ attackMoves
    ++ directAttackMoves
    ++ [ Move.EndBattlePhase, Move.EndTurn ]

battlePhase ::
  Operations e =>
  Eff e (Maybe Victory)
battlePhase = validMoves >>= chooseMove >>= \case

  Move.Attack sourceMonster targetMonster -> do
    Operation.attack L.currentPlayer sourceMonster L.otherPlayer targetMonster

  Move.DirectAttack monster -> do
    directAttack L.currentPlayer monster L.otherPlayer

  Move.EndBattlePhase -> do
    enterEndPhase
    return Nothing

  Move.EndTurn -> do
    endTurn
    return Nothing
