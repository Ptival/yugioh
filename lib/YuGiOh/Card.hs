{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Card` is a card when not in a particular position (for instance, in the
-- player's hand, or in the graveyard).  When it is played on the field, it will
-- require extra informtaion to keep track of what happens to it.
module YuGiOh.Card
  ( Card (..),
    attack,
    attribute,
    defense,
    description,
    level,
    monsterType,
    name,
  )
where

import Control.Lens
import Data.String.Interpolate
import GHC.Generics
import YuGiOh.Attribute
import YuGiOh.Classes.Displayable
import YuGiOh.Type

data Card
  = Card
      { _name :: String,
        _attribute :: Attribute,
        _level :: Int,
        _monsterType :: Type,
        _description :: String,
        _attack :: Int,
        _defense :: Int
      }
  deriving (Eq, Generic, Show)

makeLenses ''Card

instance Displayable Card where
  display Card {..} =
    [i|#{_name} [#{_attribute}] (#{_monsterType} Lv.#{_level}) [#{_attack}/#{_defense}]|]
