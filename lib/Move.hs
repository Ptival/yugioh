{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | A `Move` is any action a player can take at any point during a duel.

module Move (
  Move(..),
  Move.display,
  ) where

import Control.Lens            (view)
import Data.String.Interpolate (i)

import Card
import In
import Mat
import Phase
import Position
import Space
import Zipper

data Move (phase :: Phase) where
  DrawCard     :: In phase '[ 'Draw ] => Move phase
  EndDrawPhase :: In phase '[ 'Draw ] => Move phase
  EndMainPhase :: In phase '[ 'Main ] => Move phase
  EndTurn ::
    In phase '[ 'End, 'Main ] => Move phase
  NormalSummon ::
    In phase '[ 'Main ] =>
    Zipper Card Card -> Position -> Move phase
  SwitchPosition ::
    In phase '[ 'Main ] =>
    Zipper (Space 'IsMonsterCard) ScopedSpace -> Move phase

display :: Move p -> String
display = \case

  DrawCard -> "Draw card"

  EndDrawPhase -> "End draw phase"

  EndMainPhase -> "End main phase"

  EndTurn -> "End turn"

  NormalSummon zipper position ->
    let m = Card.display $ view cursor zipper in
    let p = Position.display position in
    [i|Normal summon #{m} in #{p}|]

  SwitchPosition zipper ->
    let c = Space.displaySpace $ view cursor zipper in
    let p = Position.display $ view (cursor . monsterPosition) zipper in
    [i|Switch #{c} to #{p}|]
