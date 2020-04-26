{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- | The driver runs a duel to completion.
module YuGiOh.Driver
  ( runDuel,
  )
where

import Control.Monad.Loops (untilJust)
import Polysemy
import Polysemy.Fail
import Polysemy.Reader (runReader)
import Polysemy.State (evalState)
import Polysemy.Writer (runWriter)
import YuGiOh.BattlePhase
import YuGiOh.ChooseOption
import YuGiOh.Configuration
import YuGiOh.DrawPhase
import YuGiOh.Duel
import YuGiOh.Duelist
import YuGiOh.EndPhase
import YuGiOh.Fresh
import qualified YuGiOh.Lenses as L
import YuGiOh.Log
import YuGiOh.MainPhase
import YuGiOh.Operation
import YuGiOh.Phase
import YuGiOh.Player
import YuGiOh.Victory

runPhase ::
  Member (Embed IO) e =>
  Member Operation e =>
  Sem e (Maybe Victory)
runPhase =
  getPhase >>= \case
    BattlePhase StartStep -> startStep
    BattlePhase BattleStep -> battleStep
    BattlePhase DamageStep -> damageStep
    BattlePhase EndStep -> endStep
    DrawPhase -> drawPhase
    EndPhase-> endPhase
    MainPhase -> mainPhase

duel ::
  Member (Embed IO) e =>
  Member Operation e =>
  Sem e Victory
duel = do
  shuffleDeck L.currentPlayer
  shuffleDeck L.otherPlayer
  untilJust runPhase

makeDuel :: Player -> Player -> Duel
makeDuel firstPlayer secondPlayer =
  Duel
    { _turn = 1,
      _phase = DrawPhase,
      _currentPlayer = firstPlayer,
      _otherPlayer = secondPlayer
    }

runDuel ::
  Member ChooseOption e =>
  Member (Embed IO) e =>
  Member Fail e =>
  Member Fresh e =>
  Duelist ->
  Duelist ->
  Sem e (Log, Victory)
runDuel duelist1 duelist2 =
  runReader duelLinksConfiguration $
    do
      player1 <- makePlayer duelist1
      player2 <- makePlayer duelist2
      let initialState = makeDuel player1 player2
      evalState initialState
        $ runWriter
        $ handleOperation duel
