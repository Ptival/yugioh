{-# LANGUAGE TemplateHaskell #-}

-- | A `Duelist` describes a duelist prior to any duel.  It contain information
-- | like their names, and the deck they'd like to use for the duel.
module YuGiOh.Duelist
  ( Duelist (..),
    deck,
    YuGiOh.Duelist.name,
  )
where

import Control.Lens
import YuGiOh.Card

data Duelist
  = Duelist
      { _name :: String,
        _deck :: [Card]
      }

makeLenses ''Duelist
