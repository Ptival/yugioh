{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  EmptySpace,
  Identifier,
  MonsterSpace,
  ScopedSpace(..),
  Space(..),
  SpaceType(..),
  destroyMonster,
  destroyMonsterByIdentifier,
  destroyMonsterWhen,
  Space.display,
  displaySpace,
  filterMonsterCards,
  Space.flip,
  hasAttacked,
  hasSwitchedPosition,
  identifier,
  isEmpty,
  isInAttackPosition,
  isInDefensePosition,
  isMonsterInAttackPosition,
  isMonsterInDefensePosition,
  monsterCard,
  monsterPosition,
  monsterSpaces,
  prepareForNewTurn,
  scoped,
  summonMonster,
  switchPosition,
  whenMonster,
  ) where

import Control.Eff             (Eff, Member)
import Control.Eff.Fresh       (Fresh, fresh)
import Control.Lens            (over, Lens', lens, set, view)
import Data.Maybe              (maybeToList)
import Data.String.Interpolate (i)
import GHC.Generics            (Generic)

import Card
import Position
import Utils

data SpaceType
  = IsEmpty
  | IsMonsterCard
  deriving (Eq, Generic, Show)

newtype Identifier = Identifier Int
  deriving (Eq)

data Space a where
  Empty       :: Space 'IsEmpty
  MonsterCard ::
    { _hasAttacked         :: Bool
    , _hasSwitchedPosition :: Bool
    , _identifier          :: Identifier
    , _monsterCard         :: Card
    , _monsterPosition     :: Position
    } ->
    Space 'IsMonsterCard

type   EmptySpace = Space 'IsEmpty
type MonsterSpace = Space 'IsMonsterCard

hasAttacked :: Lens' MonsterSpace Bool
hasAttacked = lens _hasAttacked (\ s new -> s { _hasAttacked = new })

hasSwitchedPosition :: Lens' MonsterSpace Bool
hasSwitchedPosition = lens _hasSwitchedPosition (\ s new -> s { _hasSwitchedPosition = new })

identifier :: Lens' MonsterSpace Identifier
identifier = lens _identifier (\ s new -> s { _identifier = new })

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

instance Semigroup ScopedSpace where
  (<>) _ _ = error "This should not happen"

instance Monoid ScopedSpace where
  mempty = ScopedSpace Empty

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

flip :: MonsterSpace -> MonsterSpace
flip = over monsterPosition Position.flip

filterMonsterCard :: ScopedSpace -> Maybe MonsterSpace
filterMonsterCard = scoped $ \case
    Space.Empty        -> Nothing
    s@(MonsterCard {}) -> Just s

filterMonsterCards :: [ScopedSpace] -> [MonsterSpace]
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

isInAttackPosition :: MonsterSpace -> Bool
isInAttackPosition (MonsterCard {..}) = _monsterPosition == Attack

isInDefensePosition :: MonsterSpace -> Bool
isInDefensePosition (MonsterCard {..}) = isDefensePosition _monsterPosition

monsterSpaces :: [ScopedSpace] -> [MonsterSpace]
monsterSpaces = foldr (scoped appendIfMonster) []
  where
    appendIfMonster :: Space a -> [MonsterSpace] -> [MonsterSpace]
    appendIfMonster Empty              = id
    appendIfMonster c@(MonsterCard {}) = (c :)

summonMonster :: ( Member Fresh e ) => Card -> Position -> Eff e MonsterSpace
summonMonster _monsterCard _monsterPosition = do
  freshIdentifier <- Identifier <$> fresh
  return $ MonsterCard
    { _hasAttacked         = False
    , _hasSwitchedPosition = True
    , _identifier          = freshIdentifier
    , _monsterCard
    , _monsterPosition
    }

prepareForNewTurn :: ScopedSpace -> ScopedSpace
prepareForNewTurn = scoped $ \case
  s@Space.Empty      -> ScopedSpace $ s
  -- It is better to be explicit here, so that we don't accidentally forget new fields
  s@(MonsterCard {}) -> ScopedSpace $ MonsterCard
    { _hasAttacked         = False
    , _hasSwitchedPosition = False
    , _identifier          = view identifier      s
    , _monsterCard         = view monsterCard     s
    , _monsterPosition     = view monsterPosition s
    }

switchToAttackPosition :: MonsterSpace -> MonsterSpace
switchToAttackPosition = compose
  [ set monsterPosition     Attack
  , set hasSwitchedPosition True
  ]

switchToDefensePosition :: MonsterSpace -> MonsterSpace
switchToDefensePosition = compose
  [ set monsterPosition     FaceUpDefense
  , set hasSwitchedPosition True
  ]

whenMonster :: (MonsterSpace -> MonsterSpace) -> (ScopedSpace -> ScopedSpace)
whenMonster f = scoped $ \case
  Empty              -> ScopedSpace $ Empty
  c@(MonsterCard {}) -> ScopedSpace $ f c

destroyMonsterWhen :: (MonsterSpace -> Bool) -> (ScopedSpace -> ScopedSpace)
destroyMonsterWhen cond = scoped $ \case
  Empty                          -> ScopedSpace Empty
  c@(MonsterCard {}) | cond c    -> ScopedSpace Empty
                     | otherwise -> ScopedSpace c

destroyMonster :: MonsterSpace -> (ScopedSpace -> ScopedSpace)
destroyMonster targetMonster = destroyMonsterByIdentifier (view identifier targetMonster)

destroyMonsterByIdentifier :: Identifier -> (ScopedSpace -> ScopedSpace)
destroyMonsterByIdentifier targetIdentifier =
  destroyMonsterWhen ((== targetIdentifier) . view identifier)

switchPosition :: MonsterSpace -> MonsterSpace
switchPosition s = case view monsterPosition s of
  Attack          -> switchToDefensePosition s
  FaceDownDefense -> switchToAttackPosition  s
  FaceUpDefense   -> switchToAttackPosition  s
