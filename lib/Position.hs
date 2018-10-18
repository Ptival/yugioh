{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Position` describes how a monster card is set on the field.

module Position (
  Position(..),
  display,
  isDefensePosition,
  ) where

import GHC.Generics

data Position
  = Attack
  | FaceDownDefense
  | FaceUpDefense
  deriving (Eq, Generic, Show)

display :: Position -> String
display = \case
  Attack          ->            "Attack position"
  FaceDownDefense -> "Face-Down Defense position"
  FaceUpDefense   ->   "Face-Up Defense position"

isDefensePosition :: Position -> Bool
isDefensePosition = \case
  Attack          -> False
  FaceDownDefense -> True
  FaceUpDefense   -> True
