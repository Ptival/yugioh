-- |

module Identifier (
  Identifier(..),
  ) where

newtype Identifier = Identifier Int
  deriving (Eq)
