{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Mat` encompasses all the cards that are put down to play.
module YuGiOh.Mat
  ( Deck,
    Graveyard,
    MainMonsterZone,
    Mat (..),
    Space (..),
    deck,
    graveyard,
    mainMonsterZone,
    makeMat,
    YuGiOh.Mat.prepareForNewTurn,
  )
where

import Control.Lens hiding (Empty)
import Data.String.Interpolate
import Polysemy
import Polysemy.Reader
import YuGiOh.Card
import YuGiOh.Classes.Displayable
import YuGiOh.Configuration
import YuGiOh.Space
import YuGiOh.Utils

type MainMonsterZone = [ScopedSpace]

type Graveyard = [YuGiOh.Card.Card]

type Deck = [YuGiOh.Card.Card]

data Mat
  = Mat
      { _mainMonsterZone :: MainMonsterZone,
        _graveyard :: Graveyard,
        _deck :: Deck
      }

makeLenses ''Mat

makeMat ::
  Member (Reader Configuration) e =>
  Deck ->
  Sem e Mat
makeMat playerDeck = do
  mmzSize <- askConfiguration mainMonsterZoneSize
  return $
    Mat
      { _mainMonsterZone = replicate mmzSize (ScopedSpace Empty),
        _graveyard = [],
        _deck = playerDeck
      }

instance Displayable Mat where
  display (Mat {..}) =
    let mmz = displayList display _mainMonsterZone
        d = displayList display _deck
        g = displayList display _graveyard
     in [i|Main monster zone:
#{mmz}
Deck (#{length _deck}):
#{d}
Graveyard (#{length _graveyard}):
#{g}|]

prepareForNewTurn :: Mat -> Mat
prepareForNewTurn =
  over mainMonsterZone (map YuGiOh.Space.prepareForNewTurn)
