{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Define the `Mat`, `Position`, and `Space` datatypes.

module Mat (
  Mat(..),
  Position(..),
  Space(..),
  deck,
  display,
  graveyard,
  mainMonsterZone,
  makeMat,
  ) where

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Lens              hiding (Empty)
import           Data.String.Interpolate
import           GHC.Generics

import qualified Card
import           Configuration
import           Utils

data Position
  = Attack
  | FaceDownDefense
  | FaceUpDefense
  deriving (Eq, Generic, Show)

displayPosition :: Position -> String
displayPosition = \case
  Attack          -> "Attack"
  FaceDownDefense -> "Face-Down Defense"
  FaceUpDefense   -> "Face-Up Defense"

data Space
  = Empty
  | MonsterCard Card.Card Position
  deriving (Eq, Generic, Show)

displaySpace :: Space -> String
displaySpace = \case
  Empty                     -> "Empty"
  MonsterCard card position -> [i|#{Card.display card} (#{displayPosition position})|]

data Mat = Mat
  { _mainMonsterZone :: [Space]
  , _graveyard       :: [Card.Card]
  , _deck            :: [Card.Card]
  }
  deriving (Eq, Generic, Show)

makeLenses ''Mat

makeMat ::
  ( Member (Reader Configuration) e ) =>
  [Card.Card] -> Eff e Mat
makeMat playerDeck = do
  mmzSize <- askConfiguration mainMonsterZoneSize
  return $ Mat
    { _mainMonsterZone = replicate mmzSize Empty
    , _graveyard       = []
    , _deck            = playerDeck
    }

display :: Mat -> String
display (Mat {..}) =
  let mmz = displayList displaySpace _mainMonsterZone in
  let d   = displayList Card.display _deck            in
  let g   = displayList Card.display _graveyard       in
  [i|Main monster zone:
#{mmz}
Deck (#{length _deck}):
#{d}
Graveyard (#{length _graveyard}):
#{g}|]
