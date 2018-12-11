{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Victory` describes who won a duel, and how they won it.

module Victory (
  Victory,
  VictoryCondition(..),
  Victory.display,
  makeVictory,
  ) where

import Control.Lens
import Data.String.Interpolate

import Player

data VictoryCondition
  = OpponentLPReducedToZero
  | OpponentRanOutOfCards

displayVictoryCondition :: VictoryCondition -> String
displayVictoryCondition = \case
  OpponentLPReducedToZero -> "Opponent life points reduced to zero"
  OpponentRanOutOfCards   -> "Opponent ran out of cards"

data Victory = Victory
  { _winner           :: Player
  , _victoryCondition :: VictoryCondition
  }

makeLenses ''Victory

display :: Victory -> String
display v =
  [i|#{view (winner . name) v} won: #{displayVictoryCondition $ view victoryCondition v}|]

makeVictory :: Player -> VictoryCondition -> Victory
makeVictory _winner _victoryCondition = Victory
  { _winner
  , _victoryCondition
  }
