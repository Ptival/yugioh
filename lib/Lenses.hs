{-# LANGUAGE RankNTypes #-}

-- | A bunch of useful lenses.

module Lenses (
  currentPlayer,
  currentPlayerDeck,
  currentPlayerHand,
  currentPlayerHasDrawnCard,
  currentPlayerHasNormalSummoned,
  currentPlayerMainMonsterZone,
  currentPlayerLifePoints,
  currentPlayerMat,
  otherPlayer,
  otherPlayerDeck,
  otherPlayerHand,
  otherPlayerHasDrawnCard,
  otherPlayerHasNormalSummoned,
  otherPlayerLifePoints,
  otherPlayerMainMonsterZone,
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

currentPlayerLifePoints :: Lens' Duel Int
currentPlayerLifePoints = currentPlayer . lifePoints

currentPlayerMainMonsterZone :: Lens' Duel [ScopedSpace]
currentPlayerMainMonsterZone = currentPlayerMat . mainMonsterZone

currentPlayerMat :: Lens' Duel Mat
currentPlayerMat = currentPlayer . mat

otherPlayerDeck :: Lens' Duel Deck
otherPlayerDeck = otherPlayerMat . deck

otherPlayerHand :: Lens' Duel Hand
otherPlayerHand = otherPlayer . hand

otherPlayerHasDrawnCard :: Lens' Duel Bool
otherPlayerHasDrawnCard = otherPlayer . hasDrawnCard

otherPlayerHasNormalSummoned :: Lens' Duel Bool
otherPlayerHasNormalSummoned = otherPlayer . hasNormalSummoned

otherPlayerLifePoints :: Lens' Duel Int
otherPlayerLifePoints = currentPlayer . lifePoints

otherPlayerMainMonsterZone :: Lens' Duel [ScopedSpace]
otherPlayerMainMonsterZone = otherPlayerMat . mainMonsterZone

otherPlayerMat :: Lens' Duel Mat
otherPlayerMat = otherPlayer . mat

playerDeck :: Lens' Duel Player -> Lens' Duel [Card]
playerDeck playerLens = playerLens . mat . Mat.deck

playerHand :: Lens' Duel Player -> Lens' Duel [Card]
playerHand playerLens = playerLens . hand
