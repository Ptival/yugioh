{-# LANGUAGE DeriveGeneric #-}

-- |

module Phase
  ( Phase(..)
  ) where

import GHC.Generics

data Phase
  = Draw
  | End
  deriving (Eq, Generic, Show)
