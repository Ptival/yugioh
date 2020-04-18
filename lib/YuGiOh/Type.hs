{-# LANGUAGE DeriveGeneric #-}

-- | Define the `Type` datatype.

module YuGiOh.Type (
  Type(..),
  ) where

import GHC.Generics

data Type
  = Aqua
  | Beast
  | BeastWarrior
  | CreatorGod
  | Cyberse
  | Dinosaur
  | DivineBeast
  | Dragon
  | Fairy
  | Fiend
  | Fish
  | Insect
  | Machine
  | Plant
  | Psychic
  | Pyro
  | Reptile
  | Rock
  | SeaSerpent
  | Spellcaster
  | Thunder
  | Warrior
  | WingedBeast
  | Wyrm
  | Zombie
  deriving (Eq, Generic, Show)
