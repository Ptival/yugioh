{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | During the end phase, not much happens for now.  The turn is ended, and the
-- | next turn is prepared.
module YuGiOh.EndPhase
  ( endPhase,
  )
where

import Polysemy
import YuGiOh.Operation
import Prelude hiding (log)
import YuGiOh.Victory

endPhase ::
  Member Operation e =>
  Sem e (Maybe Victory)
endPhase = do
  endTurn
  return Nothing
