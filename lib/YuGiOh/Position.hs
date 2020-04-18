{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | A `Position` describes how a monster card is set on the field.
module YuGiOh.Position
  ( Position (..),
    isDefensePosition,
    isFaceDownDefensePosition,
  )
where

import GHC.Generics
import YuGiOh.Classes.Displayable
import YuGiOh.Classes.Flippable
import Prelude hiding (flip)

data Position
  = Attack
  | FaceDownDefense
  | FaceUpDefense
  deriving (Eq, Generic, Show)

instance Displayable Position where
  display Attack = "Attack position"
  display FaceDownDefense = "Face-Down Defense position"
  display FaceUpDefense = "Face-Up Defense position"

instance Flippable Position where
  flip Attack = Attack
  flip FaceDownDefense = FaceUpDefense
  flip FaceUpDefense = FaceUpDefense

isDefensePosition :: Position -> Bool
isDefensePosition = \case
  Attack -> False
  FaceDownDefense -> True
  FaceUpDefense -> True

isFaceDownDefensePosition :: Position -> Bool
isFaceDownDefensePosition = \case
  Attack -> False
  FaceDownDefense -> True
  FaceUpDefense -> True
