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

import           Card
import           Duel
import           DuelHelpers
import           GameEffects
import           EndPhase
import qualified Lenses        as L
import           Log
import           Move
import           Phase
import           Prelude       hiding (log)
import qualified Space         as S
import           Utils
import           Victory

validMoves :: ( GameEffects e ) => Eff e [Move 'Battle]
validMoves = do
  currentPlayerMainMonsterZone <- getLensed L.currentPlayerMainMonsterZone
  otherPlayerMainMonsterZone   <- getLensed L.otherPlayerMainMonsterZone

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
  GameEffects e =>
  Eff e (Maybe Victory)
battlePhase = validMoves >>= GameEffects.chooseMove >>= \case

  Move.Attack sourceMonster targetMonster -> do
    sourcePlayer <- getLensed L.currentPlayer
    targetPlayer <- getLensed   L.otherPlayer

    overMonster L.currentPlayer sourceMonster $ set S.hasAttacked True
    log $ Attacked sourcePlayer sourceMonster targetPlayer targetMonster

    let sourceATK = view (S.monsterCard . attack)  sourceMonster

    if S.isInAttackPosition targetMonster
      then do
      let targetATK = view (S.monsterCard . attack) targetMonster
      case compare sourceATK targetATK of
        -- When the source's attack is greater than the target's attack, the
        -- target is destroyed, and damage is inflicted to the target player.
        GT -> do
          destroyMonster L.otherPlayer targetMonster
          let damage = sourceATK - targetATK
          DuelHelpers.inflictDamage L.otherPlayer damage

        -- When the source's attack is equal to the target's attack, both
        -- monsters are destroyed, but not damage is inflicted.
        EQ -> do
          destroyMonster L.currentPlayer sourceMonster
          destroyMonster   L.otherPlayer targetMonster
          return Nothing

        -- When the source's attack is lower than the target's attack, the
        -- source is destroyed, and damage is inflicted to the source player.
        LT -> do
          destroyMonster L.currentPlayer sourceMonster
          let damage = targetATK - sourceATK
          DuelHelpers.inflictDamage L.currentPlayer damage

      -- target monster is in defense position
      else do
      overMonster L.otherPlayer targetMonster $ S.flip
      let targetDEF = view (S.monsterCard . defense) targetMonster
      case compare sourceATK targetDEF of
        -- When the source's attack is greater than the target's defense, the
        -- target is destroyed, but no damage is inflicted.
        GT -> do
          destroyMonster L.otherPlayer targetMonster
          return Nothing

        -- When the source's attack is equal to the target's defense, no card
        -- is destroyed, and no damage is inflicted.
        EQ -> do
          return Nothing

        -- When the source's attack is lower than the target's defense, no card
        -- is destroyed, but damage is inflicted to the source player.
        LT -> do
          let damage = targetDEF - sourceATK
          DuelHelpers.inflictDamage L.currentPlayer damage

  Move.DirectAttack sourceMonster -> do
    overMonster L.currentPlayer sourceMonster $ set S.hasAttacked True
    let sourceATK = view (S.monsterCard . attack)  sourceMonster
    DuelHelpers.inflictDamage L.currentPlayer sourceATK

  Move.EndBattlePhase -> do
    setLensed phase End
    log Log.EndBattlePhase
    return Nothing

  Move.EndTurn -> endTurn
