{-# LANGUAGE RankNTypes #-}

-- | Helpers that can be used within a `Duel`, using `GameEffects`

module DuelHelpers
  ( DuelHelpers.destroyMonster
  , DuelHelpers.inflictDamage
  , overMonster
  , sendToGraveyard
  ) where

import Control.Eff
import Control.Lens
import Prelude hiding (log)

import Card
import Duel
import GameEffects
import Log
import Mat
import Player
import Space
import Utils
import Victory

destroyMonster ::
  GameEffects e =>
  Lens' Duel Player -> MonsterSpace -> Eff e ()
destroyMonster playerLens monster = do
  player <- getLensed playerLens
  overLensed (playerLens . mat . mainMonsterZone) $ map (Space.destroyMonster monster)
  sendToGraveyard playerLens $ view monsterCard monster
  log $ Destroyed player monster

inflictDamage ::
  GameEffects e =>
  Lens' Duel Player -> Int -> Eff e (Maybe Victory)
inflictDamage player damage = do
  overLensed player $ Player.inflictDamage damage
  damagedPlayer           <- getLensed player
  damagedPlayerLifePoints <- getLensed (player . lifePoints)
  log $ DamageInflicted damagedPlayer damage
  return $ if damagedPlayerLifePoints == 0
    then Just $ makeVictory damagedPlayer OpponentLPReducedToZero
    else Nothing

overMonster ::
  GameEffects e =>
  Lens' Duel Player -> MonsterSpace -> (MonsterSpace -> MonsterSpace) -> Eff e ()
overMonster playerLens monster f = do
  let update candidate =
        if view identifier candidate == view identifier monster
        then f candidate
        else candidate
  overLensed (playerLens . mat . mainMonsterZone) $ map (whenMonster update)

sendToGraveyard ::
  GameEffects e =>
  Lens' Duel Player -> Card -> Eff e ()
sendToGraveyard playerLens card = do
  player <- getLensed playerLens
  overLensed (playerLens . mat . graveyard) $ (:) card
  log $ SentToGraveyard player card
