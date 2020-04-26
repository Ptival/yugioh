{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | During the draw phase, players can draw a card.  Eventually, they'll be
-- able to do some additional things, like activate specific effects.
module YuGiOh.DrawPhase
  ( drawPhase,
  )
where

import Control.Monad (replicateM_)
import Polysemy
import qualified YuGiOh.Lenses as L
import YuGiOh.Move as Move
import YuGiOh.Operation
import YuGiOh.Phase
import YuGiOh.Victory
import Prelude hiding (log)

validMoves ::
  Member Operation e =>
  Sem e [Move 'DrawPhase]
validMoves =
  getHasDrawnCard L.currentPlayer >>= \case
    True -> return [Move.EndDrawPhase]
    False -> return [DrawCard]

drawPhase ::
  Member Operation e =>
  Sem e (Maybe Victory)
drawPhase =
  do
    turnNumber <- getTurnNumber
    if turnNumber == 1
      then do
        shs <- getStartingHandSize
        replicateM_ shs $ drawCard L.currentPlayer
        replicateM_ shs $ drawCard L.otherPlayer
        enter MainPhase
        return Nothing
      else validMoves >>= chooseMove >>= \case
        DrawCard ->
          drawCard L.currentPlayer
        Move.EndDrawPhase -> do
          enter MainPhase
          return Nothing
