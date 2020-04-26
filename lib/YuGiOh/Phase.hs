{-# LANGUAGE DeriveGeneric #-}

-- | A `Phase` is a phase of a duel, proceeding according to a pre-defined
-- order.
module YuGiOh.Phase
  ( BattlePhase (..),
    Phase (..),
  )
where

import GHC.Generics
import YuGiOh.Classes.Displayable

data BattlePhase
  = StartStep
  | BattleStep
  | DamageStep
  | EndStep
  deriving (Eq, Generic, Show)

instance Displayable BattlePhase where
  display StartStep = "start step"
  display BattleStep = "battle step"
  display DamageStep = "damage step"
  display EndStep = "end step"

data Phase
  = BattlePhase BattlePhase
  | DrawPhase
  | EndPhase
  | MainPhase
  deriving (Eq, Generic, Show)

instance Displayable Phase where
  display (BattlePhase step) = "Battle phase, " ++ display step
  display DrawPhase = "Draw phase"
  display EndPhase = "End phase"
  display MainPhase = "Main phase"
