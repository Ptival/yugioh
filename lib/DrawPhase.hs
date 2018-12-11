{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | During the draw phase, players can draw a card.  Eventually, they'll be
-- | able to do some additional things, like activate specific effects.

module DrawPhase (
  drawPhase,
  ) where

import           Control.Eff

import           Duel
import           GameEffects
import qualified Lenses      as L
import           Log
import           Mechanics
import           Move
import           Phase
import           Prelude     hiding (log)
import           Utils
import           Victory

validMoves :: ( GameEffects e ) => Eff e [Move 'Draw]
validMoves = do
  getLensed L.currentPlayerHasDrawnCard >>= \case
    True ->
      return [ Move.EndDrawPhase ]
    False ->
      return [ DrawCard ]

drawPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
drawPhase = validMoves >>= GameEffects.chooseMove >>= \case

  DrawCard -> do
    currentPlayerDeck <- getLensed L.currentPlayerDeck
    if length currentPlayerDeck == 0
      then do
      winner <- getLensed otherPlayer
      return $ Just $ makeVictory winner OpponentRanOutOfCards
      else do
      card <- drawCard currentPlayer
      addCardToHand currentPlayer card
      setLensed L.currentPlayerHasDrawnCard True
      drawPhase

  Move.EndDrawPhase -> do
    setLensed phase Main
    log Log.EndDrawPhase
    return Nothing
