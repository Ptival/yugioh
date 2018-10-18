{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

-- | The game mechanics are here.  When they grow too big, we'll probably want
-- | to split this file.

module Mechanics (
  addCardToHand,
  drawCard,
  ) where

import           Control.Eff
import           Control.Lens

import           Card
import           Duel
import           GameEffects
import qualified Lenses       as L
import           Log
import           Player
import           Prelude      hiding (log)
import           Utils

addCardToHand ::
  ( GameEffects e ) =>
  Lens' Duel Player -> Card -> Eff e ()
addCardToHand whichPlayer card = do
  let currentPlayerHandLens :: Lens' Duel [Card]
      currentPlayerHandLens = L.playerHand whichPlayer
  overLensed currentPlayerHandLens ((:) card)
  log =<< AddedCardToHand <$> getLensed whichPlayer <*^> card

drawCard ::
  ( GameEffects e ) =>
  Lens' Duel Player -> Eff e Card
drawCard whichPlayer = do
  let currentPlayerDeckLens :: Lens' Duel [Card]
      currentPlayerDeckLens = L.playerDeck whichPlayer
  currentPlayerDeck <- getLensed currentPlayerDeckLens
  case currentPlayerDeck of
    [] -> error "drawCard: no card left in player's deck"
    drawn : restOfDeck -> do
      setLensed currentPlayerDeckLens restOfDeck
      log =<< DrewCard <$> getLensed whichPlayer <*^> drawn
      return drawn
