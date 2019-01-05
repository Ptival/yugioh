{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Card` is a card when not in a particular position (for instance, in the
-- | player's hand, or in the graveyard).  When it is played on the field, it
-- | will require extra informtaion to keep track of what happens to it.

module Card (
  AnyCard,
  Card(..),
  MonsterCard,
  SpellCard,
  SpellType(..),
  anyCard,
  attack,
  attribute,
  defense,
  description,
  display,
  displayAny,
  displayMonster,
  displaySpell,
  filterMonsters,
  hasType,
  level,
  monsterName,
  monsterType,
  ) where

import Control.Lens
import Data.Function (on)
import Data.String.Interpolate

import Attribute
import CardEffects
import Type

data CardType
  = IsMonster
  | IsSpell

data SpellType
  = EquipSpell
  | FieldSpell
  | NormalSpell

data Card a where
  Monster ::
    { _attack      :: Int
    , _attribute   :: Attribute
    , _defense     :: Int
    , _description :: String
    , _level       :: Int
    , _monsterName :: String
    , _monsterType :: Type
    } ->
    MonsterCard
  Spell ::
    { _spellEffects :: Effects
    , _spellName    :: String
    , _spellType    :: SpellType
    } ->
    SpellCard

type MonsterCard = Card 'IsMonster
type   SpellCard = Card 'IsSpell

instance Eq MonsterCard where
  (==) = (==) `on` view monsterName

instance Show MonsterCard where
  show Monster{..} =
    [i|_monsterName|]

instance Eq SpellCard where
  (==) = (==) `on` view spellName

instance Show SpellCard where
  show Spell{..} =
    [i|_spellName|]

data AnyCard where
  AnyCard :: Card a -> AnyCard

instance Eq AnyCard where
  (==) = (==) `on` view name
  -- (==) (AnyCard m1@(Monster {})) (AnyCard m2@(Monster {})) = (==) m1 m2
  -- (==) (AnyCard s1@(Spell   {})) (AnyCard s2@(Spell   {})) = (==) s1 s2
  -- (==) (AnyCard (Monster {})) (AnyCard (Spell   {})) = False
  -- (==) (AnyCard (Spell   {})) (AnyCard (Monster {})) = False

anyCard :: Card a -> AnyCard
anyCard = AnyCard

scoped :: (forall a. Card a -> b) -> AnyCard -> b
scoped k (AnyCard s) = k s

attack :: Lens' MonsterCard Int
attack = lens _attack (\ s new -> s { _attack = new })

attribute :: Lens' MonsterCard Attribute
attribute = lens _attribute (\ s new -> s { _attribute = new })

defense :: Lens' MonsterCard Int
defense = lens _defense (\ s new -> s { _defense = new })

description :: Lens' MonsterCard String
description = lens _description (\ s new -> s { _description = new })

level :: Lens' MonsterCard Int
level = lens _level (\ s new -> s { _level = new })

monsterName :: Lens' MonsterCard String
monsterName = lens _monsterName (\ s new -> s { _monsterName = new })

monsterType :: Lens' MonsterCard Type
monsterType = lens _monsterType (\ s new -> s { _monsterType = new })

spellName :: Lens' SpellCard String
spellName = lens _spellName (\ s new -> s { _spellName = new })

name :: Lens' AnyCard String
name = lens nameOfAnyCard updateNameOfAnyCard
  where
    nameOfAnyCard :: AnyCard -> String
    nameOfAnyCard (AnyCard Monster{..}) = _monsterName
    nameOfAnyCard (AnyCard Spell{..}) = _spellName
    updateNameOfAnyCard :: AnyCard -> String -> AnyCard
    updateNameOfAnyCard (AnyCard m@Monster{}) new = AnyCard (m { _monsterName = new })
    updateNameOfAnyCard (AnyCard s@Spell{}) new = AnyCard (s { _monsterName = new })

displayMonster :: MonsterCard -> String
displayMonster Monster{..} =
  [i|#{_monsterName} [#{_attribute}] (#{_monsterType} Lv.#{_level}) [#{_attack}/#{_defense}]|]

displaySpell :: SpellCard -> String
displaySpell Spell{..} =
  [i|#{_spellName}|]

display :: Card a -> String
display = \case
  m@Monster{} -> displayMonster m
  s@Spell{}   -> displaySpell   s

displayAny :: AnyCard -> String
displayAny = scoped display

filterMonsters :: [AnyCard] -> [MonsterCard]
filterMonsters = foldr addIfMonster []
  where
    addIfMonster :: AnyCard -> [MonsterCard] -> [MonsterCard]
    addIfMonster (AnyCard m@Monster{}) = (m :)
    addIfMonster (AnyCard   Spell{})   = id

hasType :: Type -> MonsterCard -> Bool
hasType t m = (==) t (view monsterType m)
