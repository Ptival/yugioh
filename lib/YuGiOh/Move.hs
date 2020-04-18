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
import YuGiOh.Position
import YuGiOh.Space

data Move (phase :: Phase) where
  Attack ::
    In phase '[ 'Battle] =>
    MonsterSpace ->
    MonsterSpace ->
    Move phase
  DirectAttack ::
    In phase '[ 'Battle] =>
    MonsterSpace ->
    Move phase
  DrawCard ::
    In phase '[ 'Draw] =>
    Move
      phase
  EndBattlePhase ::
    In phase '[ 'Battle] =>
    Move
      phase
  EndDrawPhase ::
    In phase '[ 'Draw] =>
    Move
      phase
  EndMainPhase ::
    In phase '[ 'Main] =>
    Move
      phase
  EndTurn ::
    In phase '[ 'Battle, 'End, 'Main] =>
    Move
      phase
  NormalSummon ::
    In phase '[ 'Main] =>
    Card ->
    Position ->
    Move phase
  TributeSummon ::
    In phase '[ 'Main] =>
    Card ->
    Position ->
    Move phase
  SwitchPosition ::
    In phase '[ 'Main] =>
    MonsterSpace ->
    Move phase

instance Displayable (Move p) where
  display (YuGiOh.Move.Attack sourceSpace targetSpace) =
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
