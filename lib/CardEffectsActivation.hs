{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |

module CardEffectsActivation (
  checkActivation,
  ) where

import           Control.Eff         ( Eff )
import           Control.Monad.Loops ( allM )

import           CardEffects
import qualified Lenses              as L
import           Operation
import           Space

checkCondition ::
  Operations e =>
  Condition -> Eff e Bool
checkCondition = \case
-- checkCondition WhenThisCardIsSentFromTheFieldToTheGraveyard = return False -- FIXME

-- | Here, we assume that the card may be activated, in the sense that it is
-- either in the trap/spell zone, or there is room for it to be played.
-- Similarly, we do not check that a spell has not "just" been played here.
checkActivation ::
  Operations e =>
  Activation -> Eff e Bool

checkActivation (TargetFaceUpMonsterWithType typ) = do
    allMonsters <- getAllMonsters -- TODO: face up?
    return $ Prelude.any (Space.hasType typ) allMonsters



-- checkActivationConditions ::
--   Operations e =>
--   Effects -> Eff e Bool
-- checkActivationConditions = allM checkActivation

  -- DestroyAllMonstersOnTheField -> do
  --   allMonsters <- getAllMonsters
  --   return $ length allMonsters > 0

  -- IncreaseStatByType _ typ _ -> do
  --   allMonsters <- getAllMonsters
  --   return $ Prelude.any (Space.hasType typ) allMonsters

  -- Select1DefensePositionMonsterOnOpponentsSideAndChangeItToAttackPosition -> do
  --   otherPlayerMonsters <- monsterSpaces <$> getMainMonsterZone L.otherPlayer
  --   return $ Prelude.any isInDefensePosition otherPlayerMonsters
