{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  Space(..),
  SpellZone,
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
import Space
import Utils

type MainMonsterZone = [MonsterZoneSpace]
type SpellZone       = [SpellZoneSpace]
type Graveyard       = [AnyCard]
type Deck            = [AnyCard]

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
    { _mainMonsterZone = replicate mmzSize emptyMonsterZoneSpace
    , _graveyard       = []
    , _deck            = playerDeck
    }

newtype DisplayDeck = DisplayDeck { unDisplayDeck :: Bool }

displayDeck :: DisplayDeck -> String -> String
displayDeck = bool id (const "???") . unDisplayDeck

display :: DisplayDeck -> Mat -> String
display displayDeckFlag Mat{..} =
  let mmz = displayList Space.displayMonsterZoneSpace _mainMonsterZone in
  let d   = displayList Card.displayAny               _deck            in
  let g   = displayList Card.displayAny               _graveyard       in
  [i|Main monster zone:
#{mmz}
Deck (#{length _deck}):
#{displayDeck displayDeckFlag d}
Graveyard (#{length _graveyard}):
#{g}
|]

prepareForNewTurn :: Mat -> Mat
prepareForNewTurn =
  over mainMonsterZone (map prepareMonsterZoneSpaceForNewTurn)
