{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import Phase
import Position
import Space

data Move (phase :: Phase) where
  ActivateSpell ::
    In phase '[ 'Main ] =>
    SpellCard -> Move phase
  Attack ::
    In phase '[ 'Battle ] =>
    MonsterSpace -> MonsterSpace -> Move phase
  DirectAttack ::
    In phase '[ 'Battle ] =>
    MonsterSpace -> Move phase
  DrawCard ::
    In phase '[ 'Draw ] =>
    Move phase
  EndBattlePhase ::
    In phase '[ 'Battle ] =>
    Move phase
  EndDrawPhase ::
    In phase '[ 'Draw ] =>
    Move phase
  EndMainPhase ::
    In phase '[ 'Main ] =>
    Move phase
  EndTurn ::
    In phase '[ 'Battle, 'End, 'Main ] =>
    Move phase
  NormalSummon ::
    In phase '[ 'Main ] =>
    MonsterCard -> Position -> Move phase
  TributeSummon ::
    In phase '[ 'Main ] =>
    MonsterCard -> Position -> Move phase
  SwitchPosition ::
    In phase '[ 'Main ] =>
    MonsterSpace -> Move phase

display :: Move p -> String
display = \case

  Move.ActivateSpell spell ->
    let s = Card.display spell in
    [i|#{s}|]

  Move.Attack sourceSpace targetSpace ->
    let source = Space.display sourceSpace in
    let target = Space.display targetSpace in
    [i|#{source} attacks #{target}|]

  DirectAttack sourceSpace ->
    let source = Space.display sourceSpace in
    [i|#{source} directly attacks|]

  DrawCard -> "Draw card"

  EndBattlePhase -> "End battle phase"

  EndDrawPhase -> "End draw phase"

  EndMainPhase -> "End main phase"

  EndTurn -> "End turn"

  NormalSummon card position ->
    let m = Card.display card in
    let p = Position.display position in
    [i|Normal summon #{m} in #{p}|]

  TributeSummon card position ->
    let m = Card.displayMonster card in
    let p = Position.display position in
    [i|Tribute summon #{m} in #{p}|]

  SwitchPosition space ->
    let c = Space.display space in
    let p = Position.display $ view monsterPosition space in
    [i|Switch #{c} to #{p}|]
