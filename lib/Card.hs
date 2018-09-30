{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Define the `Card` datatype.

module Card (
  Card(..),
  attack,
  attribute,
  defense,
  description,
  display,
  level,
  monsterType,
  name,
  ) where

import Control.Lens
import Data.String.Interpolate
import GHC.Generics

import Attribute
import Type

data Card = Card
  { _name        :: String
  , _attribute   :: Attribute
  , _level       :: Int
  , _monsterType :: Type
  , _description :: String
  , _attack      :: Int
  , _defense     :: Int
  }
  deriving (Eq, Generic, Show)

makeLenses ''Card

display :: Card -> String
display (Card {..}) =
  [i|#{_name} [#{_attribute}] (#{_monsterType} Lv.#{_level}) [#{_attack}/#{_defense}]|]
