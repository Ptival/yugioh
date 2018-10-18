{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Mat` encompasses all the cards that are put down to play.

module Mat (
  Deck,
  DisplayDeck(..),
  Graveyard,
  MainMonsterZone,
  Mat(..),
  Position(..),
  Space(..),
  deck,
  Mat.display,
  graveyard,
  mainMonsterZone,
  makeMat,
  Mat.prepareForNewTurn,
  ) where

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Lens              hiding (Empty)
import Data.Bool
import Data.String.Interpolate

import Card
import Configuration
import Position
import Space
import Utils

type MainMonsterZone = [ScopedSpace]
type Graveyard       = [Card.Card]
type Deck            = [Card.Card]

data Mat = Mat
  { _mainMonsterZone :: MainMonsterZone
  , _graveyard       :: Graveyard
  , _deck            :: Deck
  }

makeLenses ''Mat

makeMat :: ( Member (Reader Configuration) e ) => Deck -> Eff e Mat
makeMat playerDeck = do
  mmzSize <- askConfiguration mainMonsterZoneSize
  return $ Mat
    { _mainMonsterZone = replicate mmzSize (ScopedSpace Empty)
    , _graveyard       = []
    , _deck            = playerDeck
    }

newtype DisplayDeck = DisplayDeck { unDisplayDeck :: Bool }

displayDeck :: DisplayDeck -> String -> String
displayDeck = bool id (const "???") . unDisplayDeck

display :: DisplayDeck -> Mat -> String
display displayDeckFlag (Mat {..}) =
  let mmz = displayList Space.display _mainMonsterZone in
  let d   = displayList  Card.display _deck            in
  let g   = displayList  Card.display _graveyard       in
  [i|Main monster zone:
#{mmz}
Deck (#{length _deck}):
#{displayDeck displayDeckFlag d}
Graveyard (#{length _graveyard}):
#{g}|]

prepareForNewTurn :: Mat -> Mat
prepareForNewTurn =
  over mainMonsterZone (map Space.prepareForNewTurn)
