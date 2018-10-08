{-# LANGUAGE TemplateHaskell #-}

-- |

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
