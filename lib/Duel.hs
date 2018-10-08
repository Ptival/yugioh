{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Duel (
  -- Duel(..),
  -- currentPlayer,
  -- otherPlayer,
  -- phase,
  -- turn,
  runDuel,
  ) where

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Writer.Strict
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import GHC.Generics

import Card
import Configuration
import Duelist
import Log
import Mat
import Phase
import Player
import Prelude                   hiding (log)
import Utils
import Victory

data Duel = Duel
  { _turn          :: Int
  , _phase         :: Phase
  , _currentPlayer :: Player
  , _otherPlayer   :: Player
  }
  deriving (Eq, Generic, Show)

makeLenses ''Duel

type GameEffects e =
  ( Member (Reader Configuration) e
  , Member (State  Duel)          e
  , Member (Writer Log)           e
  )

playerDeckLens :: Lens' Duel Player -> Lens' Duel [Card]
playerDeckLens playerLens = playerLens . mat . Mat.deck

playerHandLens :: Lens' Duel Player -> Lens' Duel [Card]
playerHandLens playerLens = playerLens . hand

drawCard ::
  ( GameEffects e ) =>
  Lens' Duel Player -> Eff e Card
drawCard whichPlayer = do
  let currentPlayerDeckLens :: Lens' Duel [Card]
      currentPlayerDeckLens = playerDeckLens whichPlayer
  currentPlayerDeck <- getLensed currentPlayerDeckLens
  case currentPlayerDeck of
    [] -> error "drawCard: no card left in player's deck"
    drawn : restOfDeck -> do
      setLensed currentPlayerDeckLens restOfDeck
      log =<< DrewCard <$> getLensed whichPlayer <*^> drawn
      return drawn

addCardToHand ::
  ( GameEffects e ) =>
  Lens' Duel Player -> Card -> Eff e ()
addCardToHand whichPlayer card = do
  let currentPlayerHandLens :: Lens' Duel [Card]
      currentPlayerHandLens = playerHandLens whichPlayer
  overLensed currentPlayerHandLens ((:) card)
  log =<< AddedCardToHand <$> getLensed whichPlayer <*^> card

endDrawPhase :: ( GameEffects e ) => Eff e ()
endDrawPhase = do
  setLensed phase End
  log EndDrawPhase

drawPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
drawPhase = do
  currentPlayerDeck <- getLensed $ playerDeckLens currentPlayer
  if length currentPlayerDeck == 0
    then do
    winner <- getLensed otherPlayer
    return $ Just $ makeVictory winner OpponentRanOutOfCards
    else do
    card <- drawCard currentPlayer
    addCardToHand currentPlayer card
    endDrawPhase
    return Nothing

endPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
endPhase = do
  finishedTurnPlayer <- getLensed currentPlayer
  upcomingTurnPlayer <- getLensed otherPlayer
  setLensed  currentPlayer upcomingTurnPlayer
  setLensed  otherPlayer   finishedTurnPlayer
  overLensed turn          (+ 1)
  setLensed  phase         Draw
  log EndTurn
  return Nothing

runPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
runPhase = do
  getLensed phase >>= \case
    Draw -> drawPhase
    End  -> endPhase

duel :: ( GameEffects e ) => Eff e Victory
duel = untilJust runPhase

makeDuel :: Player -> Player -> Duel
makeDuel firstPlayer secondPlayer = Duel
  { _turn          = 1
  , _phase         = Draw
  , _currentPlayer = firstPlayer
  , _otherPlayer   = secondPlayer
  }

runDuel :: Duelist -> Duelist -> ((Victory, Log), Duel)
runDuel duelist1 duelist2 =
  run
  $ runReader duelLinksConfiguration
  $ do
  player1 <- makePlayer duelist1
  player2 <- makePlayer duelist2
  let initialState = makeDuel player1 player2
  runState initialState $ runMonoidWriter duel
