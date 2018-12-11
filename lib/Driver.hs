{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The driver runs a duel to completion.

module Driver (
  runDuel,
  ) where

import Control.Eff               (Eff, Member)
import Control.Eff.Fresh         (runFresh')
import Control.Eff.Reader.Strict (runReader)
import Control.Eff.State.Strict  (evalState)
import Control.Eff.Writer.Strict (runMonoidWriter)
import Control.Monad.Loops       (untilJust)

import BattlePhase
import ChooseMove
import Configuration
import DrawPhase
import Duel
import Duelist
import EndPhase
import GameEffects
import Log
import MainPhase
import Phase
import Player
import Utils
import Victory

runPhase :: ( GameEffects e ) => Eff e (Maybe Victory)
runPhase = do
  getLensed phase >>= \case
    Battle -> battlePhase
    Draw   -> drawPhase
    End    -> endPhase
    Main   -> mainPhase

duel :: ( GameEffects e ) => Eff e Victory
duel = untilJust runPhase

makeDuel :: Player -> Player -> Duel
makeDuel firstPlayer secondPlayer = Duel
  { _turn          = 1
  , _phase         = Draw
  , _currentPlayer = firstPlayer
  , _otherPlayer   = secondPlayer
  }

runDuel ::
  ( Member ChooseMove e ) =>
  Duelist -> Duelist -> Eff e (Victory, Log)
runDuel duelist1 duelist2 =
  runReader duelLinksConfiguration
  $ do
  player1 <- makePlayer duelist1
  player2 <- makePlayer duelist2
  let initialState = makeDuel player1 player2
  evalState initialState $ runMonoidWriter $ runFresh' 0 duel
