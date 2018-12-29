{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Position` describes how a monster card is set on the field.

module Position (
  Position(..),
  display,
  flip,
  isDefensePosition,
  isFaceDownDefensePosition,
  ) where

import GHC.Generics

import Prelude      hiding (flip)

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

flip :: Position -> Position
flip Attack          = Attack
flip FaceDownDefense = FaceUpDefense
flip FaceUpDefense   = FaceUpDefense

isDefensePosition :: Position -> Bool
isDefensePosition = \case
  Attack          -> False
  FaceDownDefense -> True
  FaceUpDefense   -> True

isFaceDownDefensePosition :: Position -> Bool
isFaceDownDefensePosition = \case
  Attack          -> False
  FaceDownDefense -> True
  FaceUpDefense   -> True
