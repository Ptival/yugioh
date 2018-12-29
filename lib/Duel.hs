{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Duel` is the "mutable" state that evolves throughout the duel.  It does
-- | *not* contain static information, that is stored in the `Configuration`.
-- | It also does *not* contain read-only information like the duel log, which
-- | is stored separately in `Log`.

module Duel (
  Duel(..),
  PlayerLens,
  currentPlayer,
  otherPlayer,
  phase,
  turn,
  ) where

import Control.Lens

import Phase
import Player

data Duel = Duel
  { _turn          :: Int
  , _phase         :: Phase
  , _currentPlayer :: Player
  , _otherPlayer   :: Player
  }

type PlayerLens = Lens' Duel Player

makeLenses ''Duel
