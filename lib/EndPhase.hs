{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | During the end phase, not much happens for now.  The turn is ended, and the
-- | next turn is prepared.

module EndPhase (
  endPhase,
  endTurn,
  ) where

import Control.Eff

import Duel
import GameEffects
import Log
import Phase
import Player
import Prelude     hiding (log)
import Utils
import Victory

endTurn :: ( GameEffects e ) => Eff e (Maybe Victory)
endTurn = do
  finishedTurnPlayer <- getLensed currentPlayer
  upcomingTurnPlayer <- getLensed otherPlayer
  setLensed  currentPlayer $ prepareForNewTurn upcomingTurnPlayer
  setLensed  otherPlayer   finishedTurnPlayer
  overLensed turn          (+ 1)
  setLensed  phase         Draw
  log EndTurn
  return Nothing

endPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
endPhase = do
  finishedTurnPlayer <- getLensed currentPlayer
  upcomingTurnPlayer <- getLensed otherPlayer
  setLensed  currentPlayer $ prepareForNewTurn upcomingTurnPlayer
  setLensed  otherPlayer   finishedTurnPlayer
  overLensed turn          (+ 1)
  setLensed  phase         Draw
  log EndTurn
  return Nothing
