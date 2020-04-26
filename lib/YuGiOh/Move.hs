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
module YuGiOh.Move
  ( Move (..),
  )
where

import Control.Lens (view)
import Data.String.Interpolate (i)
import YuGiOh.Card
import YuGiOh.Classes.Displayable
import YuGiOh.In
import YuGiOh.Phase
import YuGiOh.Position (Position)
import YuGiOh.Space

data Move (phase :: Phase) where
  Attack ::
    In phase '[ 'BattlePhase 'BattleStep] =>
    MonsterSpace ->
    MonsterSpace ->
    Move phase
  DirectAttack ::
    In phase '[ 'BattlePhase 'BattleStep] =>
    MonsterSpace ->
    Move phase
  DrawCard ::
    In phase '[ 'DrawPhase] =>
    Move phase
  EndBattlePhase ::
    In phase '[ 'BattlePhase 'BattleStep] =>
    Move phase
  EndDrawPhase ::
    In phase '[ 'DrawPhase] =>
    Move phase
  EndMainPhase ::
    In phase '[ 'MainPhase] =>
    Move phase
  EndTurn ::
    In phase '[ 'BattlePhase 'BattleStep, 'EndPhase, 'MainPhase] =>
    Move phase
  NormalSummon ::
    In phase '[ 'MainPhase] =>
    Card ->
    Position ->
    Move phase
  TributeSummon ::
    In phase '[ 'MainPhase] =>
    Card ->
    Position ->
    Move phase
  SwitchPosition ::
    In phase '[ 'MainPhase] =>
    MonsterSpace ->
    Move phase

instance Displayable (Move p) where
  display (Attack sourceSpace targetSpace) =
    let source = display sourceSpace
        target = display targetSpace
     in [i|#{source} attacks #{target}|]
  display (DirectAttack sourceSpace) =
    let source = display sourceSpace
     in [i|#{source} directly attacks|]
  display DrawCard = "Draw card"
  display EndBattlePhase = "End battle phase"
  display EndDrawPhase = "End draw phase"
  display EndMainPhase = "End main phase"
  display EndTurn = "End turn"
  display (NormalSummon card position) =
    let m = display card
        p = display position
     in [i|Normal summon #{m} in #{p}|]
  display (TributeSummon card position) =
    let m = display card
        p = display position
     in [i|Tribute summon #{m} in #{p}|]
  display (SwitchPosition space) =
    let c = display space
        p = display $ view monsterPosition space
     in [i|Switch #{c} to #{p}|]
