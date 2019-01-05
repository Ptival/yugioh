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
  Space.EmptySpace,
  Space.MonsterSpace,
  Space.MonsterZoneSpace(..),
  Space.AnySpace,
  Space.Space(..),
  Space.SpaceType(..),
  Space.SpellSpace,
  Space.SpellZoneSpace(..),
  Space.any,
  Space.destroyMonster,
  Space.destroyMonsterByIdentifier,
  Space.destroyMonsterWhen,
  Space.display,
  Space.displayAny,
  Space.displayMonsterZoneSpace,
  Space.emptyMonsterZoneSpace,
  Space.filterMonsters,
  Space.flip,
  Space.hasAttacked,
  Space.hasType,
  Space.hasSwitchedPosition,
  Space.isEmpty,
  Space.isEmptyAny,
  Space.isEmptyMonsterZoneSpace,
  Space.isInAttackPosition,
  Space.isInDefensePosition,
  Space.isInFaceDownDefensePosition,
  Space.isMonsterInAttackPosition,
  Space.isMonsterInDefensePosition,
  Space.isSameMonster,
  Space.monsterCard,
  Space.monsterIdentifier,
  Space.monsterPosition,
  Space.monsterSpaces,
  Space.prepareMonsterZoneSpaceForNewTurn,
  Space.scoped,
  Space.spellCard,
  Space.spellEquippedTo,
  Space.spellIdentifier,
  Space.summonMonster,
  Space.switchPosition,
  Space.updateMonster,
  ) where

import Control.Eff             ( Eff, Member )
import Control.Eff.Fresh       ( Fresh, fresh )
import Control.Lens            ( over, Lens', lens, set, view )
import Data.Function           ( on )
import Data.Maybe              ( maybeToList )
import Data.String.Interpolate ( i )
import GHC.Generics            ( Generic )

import Card
import Identifier
import In
import Position
import Type
import Utils

data SpaceType
  = IsEmpty
  | IsMonster
  | IsSpell
  deriving (Eq, Generic, Show)

data Space a where
  Empty :: Space 'IsEmpty
  Monster ::
    { _hasAttacked         :: Bool
    , _hasSwitchedPosition :: Bool
    , _monsterCard         :: MonsterCard
    , _monsterIdentifier   :: Identifier
    , _monsterPosition     :: Position
    } ->
    MonsterSpace
  Spell ::
    { _spellCard       :: SpellCard
    , _spellEquippedTo :: Maybe Identifier
    , _spellIdentifier :: Identifier
    } ->
    SpellSpace

type   EmptySpace = Space 'IsEmpty
type MonsterSpace = Space 'IsMonster
type   SpellSpace = Space 'IsSpell

data MonsterZoneSpace where
  MonsterZoneSpace :: In s ['IsEmpty, 'IsMonster] => Space s -> MonsterZoneSpace

emptyMonsterZoneSpace :: MonsterZoneSpace
emptyMonsterZoneSpace = MonsterZoneSpace $ Empty

displayMonsterZoneSpace :: MonsterZoneSpace -> String
displayMonsterZoneSpace (MonsterZoneSpace s) = Space.display s

data SpellZoneSpace where
  SpellZoneSpace :: In s ['IsEmpty, 'IsSpell] => Space s -> SpellZoneSpace

hasAttacked :: Lens' MonsterSpace Bool
hasAttacked = lens _hasAttacked (\ s new -> s { _hasAttacked = new })

hasSwitchedPosition :: Lens' MonsterSpace Bool
hasSwitchedPosition = lens _hasSwitchedPosition (\ s new -> s { _hasSwitchedPosition = new })

monsterCard :: Lens' MonsterSpace MonsterCard
monsterCard = lens _monsterCard (\ s new -> s { _monsterCard = new })

monsterIdentifier :: Lens' MonsterSpace Identifier
monsterIdentifier = lens _monsterIdentifier (\ s new -> s { _monsterIdentifier = new })

monsterPosition :: Lens' MonsterSpace Position
monsterPosition = lens _monsterPosition (\ s new -> s { _monsterPosition = new })

spellCard :: Lens' SpellSpace SpellCard
spellCard = lens _spellCard (\ s new -> s { _spellCard = new })

spellEquippedTo :: Lens' SpellSpace (Maybe Identifier)
spellEquippedTo = lens _spellEquippedTo (\ s new -> s { _spellEquippedTo = new })

spellIdentifier :: Lens' SpellSpace Identifier
spellIdentifier = lens _spellIdentifier (\ s new -> s { _spellIdentifier = new })

instance Show EmptySpace where
  show Space.Empty = "Empty"

instance Show MonsterSpace where
  show (Space.Monster {..}) =
    [i|[#{show _monsterCard}] (#{show _monsterPosition})|]

instance Show SpellSpace where
  show (Space.Spell {..}) =
    [i|[#{show _spellCard}]|]

data AnySpace where
  AnySpace :: Space a -> AnySpace

any :: Space a -> AnySpace
any = AnySpace

instance Semigroup AnySpace where
  (<>) _ _ = error "This should not happen"

instance Monoid AnySpace where
  mempty = AnySpace Empty

instance Show AnySpace where
  show = scoped $ \case
    s@Space.Empty          -> show s
    s@(Space.Monster {..}) -> show s
    s@(Space.Spell   {..}) -> show s

display :: Space a -> String
display = \case
  Space.Empty        -> "Empty"
  Space.Monster {..} ->
    [i|#{Card.displayMonster _monsterCard} (#{Position.display _monsterPosition})|]
  Space.Spell {..} ->
    [i|#{Card.displaySpell _spellCard}|]

displayAny :: AnySpace -> String
displayAny = scoped Space.display

scoped :: (forall a. Space a -> b) -> AnySpace -> b
scoped k (AnySpace s) = k s

flip :: MonsterSpace -> MonsterSpace
flip = over monsterPosition Position.flip

filterMonster :: MonsterZoneSpace -> Maybe MonsterSpace
filterMonster (MonsterZoneSpace s) = case s of
    Space.Empty          -> Nothing
    m@(Space.Monster {}) -> Just m

filterMonsters :: [MonsterZoneSpace] -> [MonsterSpace]
filterMonsters l = maybeToList . filterMonster =<< l

isEmpty :: Space a -> Bool
isEmpty = \case
  Space.Empty      -> True
  Space.Monster {} -> False
  Space.Spell   {} -> False

isEmptyAny :: AnySpace -> Bool
isEmptyAny = scoped isEmpty

isEmptyMonsterZoneSpace :: MonsterZoneSpace -> Bool
isEmptyMonsterZoneSpace (MonsterZoneSpace s) = isEmpty s

isMonsterInAttackPosition :: AnySpace -> Bool
isMonsterInAttackPosition = scoped $ \case
  Space.Empty        -> False
  Space.Monster {..} -> _monsterPosition == Attack
  Space.Spell   {}   -> False

isMonsterInDefensePosition :: AnySpace -> Bool
isMonsterInDefensePosition = scoped $ \case
  Space.Empty        -> False
  Space.Monster {..} -> isDefensePosition _monsterPosition
  Space.Spell   {}   -> False

isInAttackPosition :: MonsterSpace -> Bool
isInAttackPosition (Space.Monster {..}) = _monsterPosition == Attack

isInDefensePosition :: MonsterSpace -> Bool
isInDefensePosition (Space.Monster {..}) = isDefensePosition _monsterPosition

isInFaceDownDefensePosition :: MonsterSpace -> Bool
isInFaceDownDefensePosition (Space.Monster {..}) = isFaceDownDefensePosition _monsterPosition

monsterSpaces :: [MonsterZoneSpace] -> [MonsterSpace]
monsterSpaces = foldr appendIfMonster []
  where
    appendIfMonster :: MonsterZoneSpace -> [MonsterSpace] -> [MonsterSpace]
    appendIfMonster (MonsterZoneSpace s) = case s of
      Empty                -> id
      m@(Space.Monster {}) -> (m :)

summonMonster :: ( Member Fresh e ) => MonsterCard -> Position -> Eff e MonsterSpace
summonMonster _monsterCard _monsterPosition = do
  freshIdentifier <- Identifier <$> fresh
  return $ Space.Monster
    { _hasAttacked         = False
    , _hasSwitchedPosition = True
    , _monsterIdentifier   = freshIdentifier
    , _monsterCard
    , _monsterPosition
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

transformMonster :: (MonsterSpace -> MonsterZoneSpace) -> (MonsterZoneSpace -> MonsterZoneSpace)
transformMonster f (MonsterZoneSpace s) = case s of
  Empty                -> MonsterZoneSpace Empty
  c@(Space.Monster {}) -> f c

updateMonster :: (MonsterSpace -> MonsterSpace) -> (MonsterZoneSpace -> MonsterZoneSpace)
updateMonster f = transformMonster $ MonsterZoneSpace . f

destroyMonsterWhen :: (MonsterSpace -> Bool) -> (MonsterZoneSpace -> MonsterZoneSpace)
destroyMonsterWhen cond =
  transformMonster $ \ m -> if cond m then MonsterZoneSpace Empty else MonsterZoneSpace m

destroyMonster :: MonsterSpace -> (MonsterZoneSpace -> MonsterZoneSpace)
destroyMonster targetMonster = destroyMonsterByIdentifier (view monsterIdentifier targetMonster)

destroyMonsterByIdentifier :: Identifier -> (MonsterZoneSpace -> MonsterZoneSpace)
destroyMonsterByIdentifier targetIdentifier =
  destroyMonsterWhen ((== targetIdentifier) . view monsterIdentifier)

switchMonsterSpacePosition :: MonsterSpace -> MonsterSpace
switchMonsterSpacePosition s = case view monsterPosition s of
  Attack          -> switchToDefensePosition s
  FaceDownDefense -> switchToAttackPosition  s
  FaceUpDefense   -> switchToAttackPosition  s

switchPosition :: MonsterZoneSpace -> MonsterZoneSpace
switchPosition = updateMonster switchMonsterSpacePosition

isSameMonster :: MonsterSpace -> MonsterSpace -> Bool
isSameMonster = (==) `on` (view monsterIdentifier)

prepareMonsterZoneSpaceForNewTurn :: MonsterZoneSpace -> MonsterZoneSpace
prepareMonsterZoneSpaceForNewTurn (MonsterZoneSpace s) = case s of
  Space.Empty -> emptyMonsterZoneSpace
  Space.Monster {..} ->
    MonsterZoneSpace $ Space.Monster
      { _hasAttacked         = False
      , _hasSwitchedPosition = False
      , _monsterIdentifier
      , _monsterCard
      , _monsterPosition
      }

hasType :: Type -> MonsterSpace -> Bool
hasType t = Card.hasType t . view monsterCard
