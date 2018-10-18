{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Space` is a location on the mat where a card may be placed.  We will
-- | eventually consider monster spaces, and spell/trap spaces.

module Space (
  ScopedSpace(..),
  Space(..),
  SpaceType(..),
  anyEmptyZipper,
  Space.display,
  displaySpace,
  filterMonsterCards,
  hasSwitchedPosition,
  isEmpty,
  isInAttackPosition,
  isInDefensePosition,
  isMonsterInAttackPosition,
  isMonsterInDefensePosition,
  monsterCard,
  monsterPosition,
  monsterZippers,
  monsterZipperToList,
  prepareForNewTurn,
  summonMonster,
  switchPosition,
  ) where

import Control.Lens            (Lens', lens, set, view)
import Data.List               (findIndex)
import Data.Maybe              (maybeToList)
import Data.String.Interpolate (i)
import GHC.Generics            (Generic)

import Card
import Position
import Utils
import Zipper

data SpaceType
  = IsEmpty
  | IsMonsterCard
  deriving (Eq, Generic, Show)

data Space a where
  Empty       :: Space 'IsEmpty
  MonsterCard ::
    { _hasSwitchedPosition :: Bool
    , _monsterCard         :: Card
    , _monsterPosition     :: Position
    } ->
    Space 'IsMonsterCard

type   EmptySpace = Space 'IsEmpty
type MonsterSpace = Space 'IsMonsterCard

type   EmptyZipper = Zipper EmptySpace   ScopedSpace
type MonsterZipper = Zipper MonsterSpace ScopedSpace

hasSwitchedPosition :: Lens' MonsterSpace Bool
hasSwitchedPosition = lens _hasSwitchedPosition (\ s new -> s { _hasSwitchedPosition = new })

monsterCard :: Lens' MonsterSpace Card
monsterCard = lens _monsterCard (\ s new -> s { _monsterCard = new })

monsterPosition :: Lens' MonsterSpace Position
monsterPosition = lens _monsterPosition (\ s new -> s { _monsterPosition = new })

instance Show EmptySpace where
  show Space.Empty = "Empty"

instance Show MonsterSpace where
  show (MonsterCard {..}) =
    [i|MonsterCard (#{show _monsterCard}) (#{show _monsterPosition})|]

data ScopedSpace where
  ScopedSpace :: Space a -> ScopedSpace

instance Show ScopedSpace where
  show = scoped $ \case
    s@Space.Empty        -> show s
    s@(MonsterCard {..}) -> show s

displaySpace :: Space a -> String
displaySpace = \case
  Space.Empty      -> "Empty"
  MonsterCard {..} ->
    [i|#{Card.display _monsterCard} (#{Position.display _monsterPosition})|]

scoped :: (forall a. Space a -> b) -> ScopedSpace -> b
scoped k (ScopedSpace s) = k s

display :: ScopedSpace -> String
display = scoped displaySpace

filterMonsterCard :: ScopedSpace -> Maybe MonsterSpace
filterMonsterCard = scoped $ \case
    Space.Empty        -> Nothing
    s@(MonsterCard {}) -> Just s

filterMonsterCards :: [ScopedSpace] -> [Space 'IsMonsterCard]
filterMonsterCards l = maybeToList . filterMonsterCard =<< l

isEmpty :: ScopedSpace -> Bool
isEmpty = scoped $ \case
  Space.Empty    -> True
  MonsterCard {} -> False

isMonsterInAttackPosition :: ScopedSpace -> Bool
isMonsterInAttackPosition = scoped $ \case
  Space.Empty      -> False
  MonsterCard {..} -> _monsterPosition == Attack

isMonsterInDefensePosition :: ScopedSpace -> Bool
isMonsterInDefensePosition = scoped $ \case
  Space.Empty      -> False
  MonsterCard {..} -> isDefensePosition _monsterPosition

isInAttackPosition :: Space 'IsMonsterCard -> Bool
isInAttackPosition (MonsterCard {..}) = _monsterPosition == Attack

isInDefensePosition :: Space 'IsMonsterCard -> Bool
isInDefensePosition (MonsterCard {..}) = isDefensePosition _monsterPosition

zipperToScopedSpaces :: Zipper (Space a) ScopedSpace -> [ScopedSpace]
zipperToScopedSpaces = toList ScopedSpace id

monsterZippers :: [ScopedSpace] -> [MonsterZipper]
monsterZippers =
  let select :: [ScopedSpace] -> forall a. Space a -> [MonsterZipper]
      select t = \case
        Space.Empty        -> []
        s@(MonsterCard {}) -> [Zipper [] s 0 t]
  in allSelectZippers (\ h t -> scoped (select t) h)

monsterZipperToList :: MonsterZipper -> [ScopedSpace]
monsterZipperToList = zipperToScopedSpaces

summonMonster :: Card -> Position -> Space 'IsMonsterCard
summonMonster _monsterCard _monsterPosition = MonsterCard
  { _hasSwitchedPosition = True
  , _monsterCard
  , _monsterPosition
  }

prepareForNewTurn :: ScopedSpace -> ScopedSpace
prepareForNewTurn = scoped $ \case
  s@Space.Empty      -> ScopedSpace $ s
  s@(MonsterCard {}) -> ScopedSpace $ set hasSwitchedPosition False s

switchToAttackPosition :: Space 'IsMonsterCard -> Space 'IsMonsterCard
switchToAttackPosition = compose
  [ set monsterPosition     Attack
  , set hasSwitchedPosition True
  ]

switchToDefensePosition :: Space 'IsMonsterCard -> Space 'IsMonsterCard
switchToDefensePosition = compose
  [ set monsterPosition     FaceUpDefense
  , set hasSwitchedPosition True
  ]

switchPosition :: Space 'IsMonsterCard -> Space 'IsMonsterCard
switchPosition s = case view monsterPosition s of
  Attack          -> switchToDefensePosition s
  FaceDownDefense -> switchToAttackPosition  s
  FaceUpDefense   -> switchToAttackPosition  s

refineIfEmpty :: ScopedSpace -> Maybe EmptySpace
refineIfEmpty = scoped $ \case
  Space.Empty      -> Just Space.Empty
  (MonsterCard {}) -> Nothing

anyEmptyZipper :: [ScopedSpace] -> Maybe EmptyZipper
anyEmptyZipper spaces = do
  emptyIndex <- findIndex isEmpty spaces
  zipper     <- zipperAtIndex emptyIndex spaces
  refined    <- refineIfEmpty (view cursor zipper)
  return $ set cursor refined zipper
