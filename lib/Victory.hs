{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Victory (
  Victory,
  VictoryCondition(..),
  Victory.display,
  makeVictory,
  ) where

import Control.Lens
import Data.String.Interpolate
import GHC.Generics

import Player

data VictoryCondition
  = OpponentRanOutOfCards
  deriving (Eq, Generic, Show)

displayVictoryCondition :: VictoryCondition -> String
displayVictoryCondition = \case

  OpponentRanOutOfCards -> "Opponent ran out of cards"

data Victory = Victory
  { _winner           :: Player
  , _victoryCondition :: VictoryCondition
  }
  deriving (Eq, Generic, Show)

makeLenses ''Victory

display :: Victory -> String
display v =
  [i|#{view (winner . name) v} won: #{displayVictoryCondition $ view victoryCondition v}|]

makeVictory :: Player -> VictoryCondition -> Victory
makeVictory _winner _victoryCondition = Victory
  { _winner
  , _victoryCondition
  }
