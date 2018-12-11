{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

-- | `GameEffects` captures all those effects we need to run a duel.

module GameEffects (
  GameEffects,
  GameEffects.chooseMove,
  ) where

import Control.Eff               (Eff, type (<::))
import Control.Eff.Fresh         (Fresh)
import Control.Eff.Reader.Strict (Reader)
import Control.Eff.State.Strict  (State, get)
import Control.Eff.Writer.Strict (Writer)

import ChooseMove
import Configuration
import Duel
import Log
import Move

type GameEffects e =
  [ ChooseMove
  , Fresh
  , Reader     Configuration
  , State      Duel
  , Writer     Log
  ] <:: e

chooseMove :: ( GameEffects e ) => [Move p] -> Eff e (Move p)
chooseMove validMoves = do
  duel <- get
  ChooseMove.chooseMove duel validMoves
