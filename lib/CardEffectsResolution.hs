{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |

module CardEffectsResolution (
  resolveSpellEffect,
  ) where

import           Control.Eff  (Eff)

import           CardEffects
import           GameEffects
import           Victory

resolveSpellEffect ::
  GameEffects e =>
  Resolution a -> a -> Eff e (Maybe Victory)
resolveSpellEffect Equip identifier = _
resolveSpellEffect (EquippedGainsATK atk) () = _
resolveSpellEffect (EquippedGainsDEF def) () = _
