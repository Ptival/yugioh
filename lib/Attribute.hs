{-# LANGUAGE DeriveGeneric #-}

-- | Define the `Attribute` datatype.

module Attribute (
  Attribute(..),
  ) where

import GHC.Generics

data Attribute
  = Dark
  | Divine
  | Earth
  | Fire
  | Light
  | Water
  | Wind
  deriving (Eq, Generic, Show)
