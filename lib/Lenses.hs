{-# LANGUAGE RankNTypes #-}

-- | A bunch of useful lenses.

module Lenses (
  currentPlayer,
  currentPlayerDeck,
  currentPlayerHand,
  currentPlayerHasDrawnCard,
  currentPlayerHasNormalSummoned,
  currentPlayerMainMonsterZone,
  currentPlayerMat,
  otherPlayer,
  otherPlayerMat,
  playerDeck,
  playerHand,
  phase,
  ) where

import Control.Lens

import Card
import Duel
import Mat
import Player
import Space

currentPlayerDeck :: Lens' Duel Deck
currentPlayerDeck = currentPlayerMat . deck

currentPlayerHand :: Lens' Duel Hand
currentPlayerHand = currentPlayer . hand

currentPlayerHasDrawnCard :: Lens' Duel Bool
currentPlayerHasDrawnCard = currentPlayer . hasDrawnCard

currentPlayerHasNormalSummoned :: Lens' Duel Bool
currentPlayerHasNormalSummoned = currentPlayer . hasNormalSummoned

currentPlayerMainMonsterZone :: Lens' Duel [ScopedSpace]
currentPlayerMainMonsterZone = currentPlayerMat . mainMonsterZone

currentPlayerMat :: Lens' Duel Mat
currentPlayerMat = currentPlayer . mat

otherPlayerMat :: Lens' Duel Mat
otherPlayerMat = otherPlayer . mat

playerDeck :: Lens' Duel Player -> Lens' Duel [Card]
playerDeck playerLens = playerLens . mat . Mat.deck

playerHand :: Lens' Duel Player -> Lens' Duel [Card]
playerHand playerLens = playerLens . hand
