{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Define the `Configuration` datatype.

module Configuration (
  Configuration(..),
  askConfiguration,
  deckMaximumSize,
  deckMinimumSize,
  duelLinksConfiguration,
  mainMonsterZoneSize,
  originalLifePoints,
  spellTrapPendulumZoneSize,
  testingConfiguration,
  tradingCardGameConfiguration,
  ) where

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Lens
import GHC.Generics

data Configuration = Configuration
  { _deckMinimumSize           :: Int
  , _deckMaximumSize           :: Int
  , _mainMonsterZoneSize       :: Int
  , _originalLifePoints        :: Int
  , _spellTrapPendulumZoneSize :: Int
  }
  deriving (Eq, Generic, Show)

makeLenses ''Configuration

duelLinksConfiguration :: Configuration
duelLinksConfiguration = Configuration
  { _deckMinimumSize           = 20
  , _deckMaximumSize           = 40
  , _mainMonsterZoneSize       = 3
  , _originalLifePoints        = 4000
  , _spellTrapPendulumZoneSize = 3
  }

testingConfiguration :: Configuration
testingConfiguration = Configuration
  { _deckMinimumSize           = 1
  , _deckMaximumSize           = 3
  , _mainMonsterZoneSize       = 3
  , _originalLifePoints        = 4000
  , _spellTrapPendulumZoneSize = 3
  }

tradingCardGameConfiguration :: Configuration
tradingCardGameConfiguration = Configuration
  { _deckMinimumSize           = 40
  , _deckMaximumSize           = 60
  , _mainMonsterZoneSize       = 5
  , _originalLifePoints        = 8000
  , _spellTrapPendulumZoneSize = 5
  }

askConfiguration ::
  ( Member (Reader Configuration) e ) =>
  Lens' Configuration a -> Eff e a
askConfiguration l = view l <$> ask
