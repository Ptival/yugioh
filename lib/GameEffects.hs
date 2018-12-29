{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | `GameEffects` captures all those effects we need to run a duel.

module GameEffects (
  GameEffects,
  chooseTribute,
  ) where

import Control.Eff               (Eff, Lifted, type (<::))
import Control.Eff.Fresh         (Fresh)
import Control.Eff.Reader.Strict (Reader)
import Control.Eff.State.Strict  (State, get)
import Control.Eff.Writer.Strict (Writer)
import Data.String.Interpolate

import ChooseOption
import Configuration
import Duel
import Log
import Space

type GameEffects e =
  (
    [ ChooseOption
    , Fresh
    , Reader       Configuration
    , State        Duel
    , Writer       Log
    ] <:: e
  , Lifted IO e
  )

chooseTribute :: ( GameEffects e ) => [MonsterSpace] -> Eff e MonsterSpace
chooseTribute tributes = do
  duel <- get
  chooseOption duel displayTribute tributes
  where
    displayTribute :: MonsterSpace -> String
    displayTribute monster = [i|Tribute #{Space.displaySpace monster}|]
