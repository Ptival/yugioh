{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Location` is a place where a card may be in a given player's side of the
-- | field.  It should unambiguously pinpoint a card.

module Location (
  CardLocation(..),
  Location(..),
  Location.display,
  displayCardLocation,
  isValid,
  ) where

import Control.Lens
import Data.String.Interpolate

import Mat
import Player

data CardLocation
  = Deck            Int
  | Graveyard       Int
  | Hand            Int
  | MainMonsterZone Int

displayCardLocation :: CardLocation -> String
displayCardLocation = \case
  Deck            n -> [i|Deck (position #{n + 1})|]
  Graveyard       n -> [i|Graveyard (position #{n + 1})|]
  Hand            n -> [i|Hand (position #{n + 1})|]
  MainMonsterZone n -> [i|MainMonsterZone (position #{n + 1})|]

data Location = Location
  { _side         :: Player
  , _cardLocation :: CardLocation
  }

makeLenses ''Location

display :: Location -> String
display location =
  [i|#{view (side . name) location}'s side, #{displayCardLocation (view cardLocation location)}'|]

isValid :: Location -> Bool
isValid location =
  let player          = view side location          in
  let checkLength n l = length (view l player) >= n in
  case view cardLocation location of
  Deck            n -> checkLength n (mat . deck)
  Graveyard       n -> checkLength n (mat . graveyard)
  Hand            n -> checkLength n hand
  MainMonsterZone n -> checkLength n (mat . mainMonsterZone)
