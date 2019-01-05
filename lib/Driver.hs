{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The driver runs a duel to completion.

module Driver (
  runDuel,
  ) where

import           Control.Eff               (Eff, Lifted, Member)
import           Control.Eff.Fresh         (runFresh')
import           Control.Eff.Reader.Strict (runReader)
import           Control.Eff.State.Strict  (evalState)
import           Control.Eff.Writer.Strict (runMonoidWriter)
import           Control.Monad             (replicateM_)
import           Control.Monad.Loops       (untilJust)

import           BattlePhase
import           ChooseOption
import           Configuration
import           DrawPhase
import           Duel
import           Duelist
import           EndPhase
import qualified Lenses                    as L
import           Log
import           MainPhase
import           Operation
import           Phase
import           Player
import           Victory

runPhase ::
  Operations e =>
  Eff e (Maybe Victory)
runPhase =
  getPhase >>= \case
    Battle -> battlePhase
    Draw   -> drawPhase
    End    -> endPhase
    Main   -> mainPhase

duel ::
  Operations e =>
  Eff e Victory
duel = do
  shuffleDeck L.currentPlayer
  shuffleDeck L.otherPlayer
  handSize <- getStartingHandSize
  replicateM_ handSize $ drawCard L.currentPlayer
  replicateM_ handSize $ drawCard L.otherPlayer
  endTurn
  untilJust runPhase

makeDuel :: Player -> Player -> Duel
makeDuel firstPlayer secondPlayer = Duel
  { _turn          = 0
  , _phase         = End
  , _currentPlayer = secondPlayer
  , _otherPlayer   = firstPlayer
  }

runDuel ::
  Member ChooseOption e =>
  Lifted IO           e =>
  Duelist -> Duelist -> Eff e (Victory, Log)
runDuel duelist1 duelist2 =
  runReader duelLinksConfiguration
  $ runFresh' 0
  $ do
  player1 <- makePlayer duelist1
  player2 <- makePlayer duelist2
  let initialState = makeDuel player1 player2
  evalState initialState
    $ runMonoidWriter
    $ handleOperation duel
