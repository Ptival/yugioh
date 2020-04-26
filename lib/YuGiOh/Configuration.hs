{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Define the @Configuration@ datatype.
module YuGiOh.Configuration
  ( Configuration (..),
    askConfiguration,
    deckMaximumSize,
    deckMinimumSize,
    duelLinksConfiguration,
    mainMonsterZoneSize,
    originalLifePoints,
    spellTrapPendulumZoneSize,
    startingHandSize,
    testingConfiguration,
    tcgConfiguration,
  )
where

import Control.Lens
import GHC.Generics
import Polysemy
import Polysemy.Reader

data Configuration
  = Configuration
      { _deckMinimumSize :: Int,
        _deckMaximumSize :: Int,
        _mainMonsterZoneSize :: Int,
        _originalLifePoints :: Int,
        _spellTrapPendulumZoneSize :: Int,
        _startingHandSize :: Int
      }
  deriving (Eq, Generic, Show)

makeLenses ''Configuration

duelLinksConfiguration :: Configuration
duelLinksConfiguration =
  Configuration
    { _deckMinimumSize = 20,
      _deckMaximumSize = 40,
      _mainMonsterZoneSize = 3,
      _originalLifePoints = 4000,
      _spellTrapPendulumZoneSize = 3,
      _startingHandSize = 4
    }

testingConfiguration :: Configuration
testingConfiguration =
  Configuration
    { _deckMinimumSize = 1,
      _deckMaximumSize = 3,
      _mainMonsterZoneSize = 3,
      _originalLifePoints = 4000,
      _spellTrapPendulumZoneSize = 3,
      _startingHandSize = 4
    }

tcgConfiguration :: Configuration
tcgConfiguration =
  Configuration
    { _deckMinimumSize = 40,
      _deckMaximumSize = 60,
      _mainMonsterZoneSize = 5,
      _originalLifePoints = 8000,
      _spellTrapPendulumZoneSize = 5,
      _startingHandSize = 5
    }

askConfiguration ::
  Member (Reader Configuration) e =>
  Lens' Configuration a ->
  Sem e a
askConfiguration l = view l <$> ask
