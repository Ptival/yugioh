{-# LANGUAGE DeriveGeneric #-}

-- | A `Phase` is a phase of a duel, proceeding according to a pre-defined
-- | order.

module Phase
  ( Phase(..)
  ) where

import GHC.Generics

data Phase
  = Draw
  | End
  | Main
  deriving (Eq, Generic, Show)
