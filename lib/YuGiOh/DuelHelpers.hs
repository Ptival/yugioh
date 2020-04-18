{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Helpers that can be used within a `Duel`, using `GameEffects`
module YuGiOh.DuelHelpers
  ( opponentOf,
    overMonster,
  )
where

import Control.Lens (ALens')
import Polysemy
import Polysemy.State
import YuGiOh.Duel
import qualified YuGiOh.Lenses as L
import YuGiOh.Mat
import YuGiOh.Player
import YuGiOh.Space
import YuGiOh.Utils
import Prelude hiding (log)

opponentOf ::
  Member (State Duel) e =>
  PlayerLens ->
  Sem e (ALens' Duel Player)
opponentOf player = do
  source <- getLensed player
  current <- getLensed L.currentPlayer
  other <- getLensed L.otherPlayer
  if isSamePlayer source current
    then return L.otherPlayer
    else
      if isSamePlayer source other
        then return L.currentPlayer
        else error "opponentOf: neither Player matched the source"

overMonster ::
  Member (State Duel) e =>
  PlayerLens ->
  MonsterSpace ->
  (MonsterSpace -> MonsterSpace) ->
  Sem e ()
overMonster playerLens monster f = do
  let update candidate =
        if isSameMonster candidate monster
          then f candidate
          else candidate
  overLensed (playerLens . mat . mainMonsterZone) $ map (whenMonster update)
