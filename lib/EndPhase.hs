{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | During the end phase, not much happens for now.  The turn is ended, and the
-- | next turn is prepared.

module EndPhase (
  endPhase,
  ) where

import Control.Eff

import Operation
import Prelude     hiding (log)
import Victory

endPhase ::
  Operations e =>
  Eff e (Maybe Victory)
endPhase = do
  endTurn
  return Nothing
