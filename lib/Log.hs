{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A `Log` is a list of `Entry` entries that describe what happens through a
-- | duel.

module Log (
  Entry(..),
  Log,
  Log.display,
  ) where

import Control.Lens
import Data.String.Interpolate

import Card
import Player
import Position
import Space

type Log = [Entry]

data Entry
  = AddedCardToHand  Player Card
  | Attacked         Player MonsterSpace Player MonsterSpace
  | DamageInflicted  Player Int
  | Destroyed        Player MonsterSpace
  | DirectAttacked   Player MonsterSpace Player
  | DrewCard         Player Card
  | EndBattlePhase
  | EndDrawPhase
  | EndMainPhase
  | EndTurn
  | NormalSummoned   Player MonsterSpace Int
  | SentToGraveyard  Player Card
  | SwitchedPosition Player MonsterSpace Int

display :: Entry -> String
display = \case

  AddedCardToHand player card ->
    [i|#{view Player.name player} added card #{Card.display card} to their hand|]

  Attacked sourcePlayer sourceMonster targetPlayer targetMonster ->
    let sp  = view Player.name sourcePlayer in
    let sm  = displaySpace sourceMonster in
    let tp  = view Player.name targetPlayer in
    let tm  = displaySpace targetMonster in
    [i|#{sp}'s #{sm} attacked #{tp}'s #{tm}|]

  DamageInflicted player damage ->
    [i|#{view Player.name player} receives #{show damage} damage|]

  Destroyed player monster ->
    [i|#{view Player.name player}'s #{displaySpace monster} got destroyed|]

  DirectAttacked sourcePlayer sourceMonster targetPlayer ->
    let sp  = view Player.name sourcePlayer in
    let sm  = displaySpace sourceMonster in
    let tp  = view Player.name targetPlayer in
    [i|#{sp}'s #{sm} attacked #{tp} directly|]

  DrewCard player card ->
    [i|#{view Player.name player} drew card #{Card.display card}|]

  EndBattlePhase -> "Battle phase ended"

  EndDrawPhase -> "Draw phase ended"

  EndMainPhase -> "Main phase ended"

  EndTurn -> "Turn ended"

  NormalSummoned player space location ->
    let pl      = view Player.name player in
    let monster = Card.display $ view monsterCard space in
    let pos     = Position.display $ view monsterPosition space in
    [i|#{pl} normal summoned #{monster} (at #{location}) in #{pos}|]

  SentToGraveyard player card ->
    let pl = view Player.name player in
    let c  = Card.display card in
    [i|#{pl}'s #{c} was sent to the graveyard|]

  SwitchedPosition player space location ->
    let pl      = view Player.name player in
    let monster = Card.display $ view monsterCard space in
    let pos     = Position.display $ view monsterPosition space in
    [i|#{pl} switched #{monster} (at #{location}) to #{pos}|]
