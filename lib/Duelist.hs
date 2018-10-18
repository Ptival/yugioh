{-# LANGUAGE TemplateHaskell #-}

-- | A `Duelist` describes a duelist prior to any duel.  It contain information
-- | like their names, and the deck they'd like to use for the duel.

module Duelist (
  Duelist(..),
  deck,
  Duelist.name,
  )where

import Control.Lens

import Card

data Duelist = Duelist
  { _name :: String
  , _deck :: [Card]
  }

makeLenses ''Duelist
