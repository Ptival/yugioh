{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module Log (
  Entry(..),
  Log,
  Log.display,
  ) where

import Control.Lens
import Data.String.Interpolate
import GHC.Generics

import Card
import Player

type Log = [Entry]

data Entry
  = AddedCardToHand Player Card
  | DrewCard        Player Card
  | EndDrawPhase
  | EndTurn
  deriving (Eq, Generic, Show)

display :: Entry -> String
display = \case

  AddedCardToHand player card ->
    [i|#{view Player.name player} added card #{Card.display card} to their hand|]

  DrewCard player card ->
    [i|#{view Player.name player} drew card #{Card.display card}|]

  EndDrawPhase -> "Draw Phase ended"

  EndTurn -> "Turn ended"
