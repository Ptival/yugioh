{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module CardEffects (
  Activation(..),
  Condition,
  Effect,
  Effects,
  Resolution(..),
  ) where

-- import Control.Lens
import GHC.Generics

import Identifier
import Type

data Condition
  -- = WhenThisCardIsSentFromTheFieldToTheGraveyard
  deriving ( Generic )

data Activation :: * -> * where
  Activate                    ::         Activation ()
  TargetFaceUpMonster         ::         Activation Identifier
  TargetFaceUpMonsterWithType :: Type -> Activation Identifier

data Resolution :: * -> * where
  Equip            ::        Resolution Identifier
  EquippedGainsATK :: Int -> Resolution ()
  EquippedGainsDEF :: Int -> Resolution ()

data Effect a = Effect
  { _condition  :: [Condition]
  , _activation :: Activation a
  , _resolution :: Resolution a
  }
  deriving ( Generic )

-- makeLenses ''Effect

data AnyEffect where
  -- AnyEffect :: Effect a -> AnyEffect

type Effects = [AnyEffect]

-- anyEffect :: [Condition] -> Activation a -> Resolution a -> AnyEffect
-- anyEffect c a r = AnyEffect (Effect c a r)

-- darkEnergyEffects :: Effects
-- darkEnergyEffects =
--   [ anyEffect [] (TargetFaceUpMonsterWithType Fiend) Equip
--   , anyEffect [] Activate                            (EquippedGainsATK 300)
--   , anyEffect [] Activate                            (EquippedGainsDEF 300)
--   ]
