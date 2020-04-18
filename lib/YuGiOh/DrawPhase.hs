{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | During the draw phase, players can draw a card.  Eventually, they'll be
-- | able to do some additional things, like activate specific effects.
module YuGiOh.DrawPhase
  ( drawPhase,
  )
where

import Polysemy
import qualified YuGiOh.Lenses as L
import YuGiOh.Move as Move
import YuGiOh.Operation
import YuGiOh.Phase
import Prelude hiding (log)
import YuGiOh.Victory

validMoves ::
  Member Operation e =>
  Sem e [Move 'Draw]
validMoves = do
  getHasDrawnCard L.currentPlayer >>= \case
    True -> return [Move.EndDrawPhase]
    False -> return [DrawCard]

drawPhase ::
  Member Operation e =>
  Sem e (Maybe Victory)
drawPhase = validMoves >>= chooseMove >>= \case
  DrawCard -> do
    drawCard L.currentPlayer
  Move.EndDrawPhase -> do
    enterMainPhase
    return Nothing
