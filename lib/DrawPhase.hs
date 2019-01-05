{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | During the draw phase, players can draw a card.  Eventually, they'll be
-- | able to do some additional things, like activate specific effects.

module DrawPhase (
  drawPhase,
  ) where

import           Control.Eff

import qualified Lenses      as L
import           Move
import           Operation
import           Phase
import           Prelude     hiding (log)
import           Victory

validMoves ::
  Operations e =>
  Eff e [Move 'Draw]
validMoves =
  getHasDrawnCard L.currentPlayer >>= \case
    True  -> return [ Move.EndDrawPhase ]
    False -> return [ DrawCard ]

drawPhase ::
  Operations e =>
  Eff e (Maybe Victory)
drawPhase = validMoves >>= chooseMove >>= \case

  DrawCard ->
    drawCard L.currentPlayer

  Move.EndDrawPhase -> do
    enterMainPhase
    return Nothing
