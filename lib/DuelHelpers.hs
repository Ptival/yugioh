{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Helpers that can be used within a `Duel`, using `GameEffects`

module DuelHelpers
  ( opponentOf
  , DuelHelpers.overMonster
  , tributeFor
  ) where

import           Control.Eff  (Eff)
import           Control.Lens (ALens')
import           Prelude      hiding (log)

import           Duel
import           GameEffects
import qualified Lenses       as L
import           Mat
import           Player
import           Space
import           Utils

opponentOf ::
  GameEffects e =>
  PlayerLens -> Eff e (ALens' Duel Player)
opponentOf player = do
  source  <- getLensed player
  current <- getLensed L.currentPlayer
  other   <- getLensed L.otherPlayer
  if isSamePlayer source current
    then return L.otherPlayer
    else if isSamePlayer source other
         then return L.currentPlayer
         else error "opponentOf: neither Player matched the source"

overMonster ::
  GameEffects e =>
  PlayerLens -> MonsterSpace -> (MonsterSpace -> MonsterSpace) -> Eff e ()
overMonster playerLens monster f = do
  let update candidate =
        if isSameMonster candidate monster
        then f candidate
        else candidate
  overLensed (playerLens . mat . mainMonsterZone) $ map (Space.updateMonster update)

tributeFor :: Int -> Int
tributeFor n | n >= 5 && n <= 6 = 1
             | n >= 7 && n <= 8 = 2
             | n >= 9           = 3
             | otherwise        = error "Only monsters above level 4 can be tribute summoned"
