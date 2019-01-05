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
  = Attacked         Player MonsterSpace Player MonsterSpace
  | DamageInflicted  Player Int
  | Destroyed        Player MonsterSpace
  | DirectAttacked   Player MonsterSpace Player
  | DrewCard         Player AnyCard
  | EndBattlePhase
  | EndDrawPhase
  | EndMainPhase
  | Flipped          Player MonsterSpace
  | NormalSummoned   Player MonsterSpace Int
  | TributeSummoned  Player MonsterSpace Int
  | SentToGraveyard  Player AnyCard
  | SwitchedPosition Player MonsterSpace Int
  | Turn             Int    Player       Player

display :: Entry -> String
display = \case

  Attacked sourcePlayer sourceMonster targetPlayer targetMonster ->
    let sp  = view Player.name sourcePlayer in
    let sm  = Space.display sourceMonster in
    let tp  = view Player.name targetPlayer in
    let tm  = Space.display targetMonster in
    [i|#{sp}'s #{sm} attacked #{tp}'s #{tm}|]

  DamageInflicted player damage ->
    [i|#{view Player.name player} receives #{show damage} damage|]

  Destroyed player monster ->
    [i|#{view Player.name player}'s #{Space.display monster} got destroyed|]

  DirectAttacked sourcePlayer sourceMonster targetPlayer ->
    let sp  = view Player.name sourcePlayer in
    let sm  = Space.display sourceMonster in
    let tp  = view Player.name targetPlayer in
    [i|#{sp}'s #{sm} attacked #{tp} directly|]

  DrewCard player card ->
    [i|#{view Player.name player} drew #{Card.displayAny card}|]

  EndBattlePhase -> "Battle phase ended"

  EndDrawPhase -> "Draw phase ended"

  EndMainPhase -> "Main phase ended"

  Flipped player monster ->
    let pl =                view Player.name player  in
    let m  = Card.display $ view monsterCard monster in
    [i|#{pl}'s #{m} got flipped|]

  NormalSummoned player monster location ->
    let pl  =                    view Player.name     player  in
    let m   =     Card.display $ view monsterCard     monster in
    let pos = Position.display $ view monsterPosition monster in
    [i|#{pl} normal summoned #{m} (at #{location}) in #{pos}|]

  TributeSummoned player monster location ->
    let pl  =                    view Player.name     player  in
    let m   =     Card.display $ view monsterCard     monster in
    let pos = Position.display $ view monsterPosition monster in
    [i|#{pl} tribute summoned #{m} (at #{location}) in #{pos}|]

  SentToGraveyard player card ->
    let pl = view Player.name player in
    let c  = Card.displayAny card in
    [i|#{pl}'s #{c} was sent to the graveyard|]

  SwitchedPosition player space location ->
    let pl      = view Player.name player in
    let monster = Card.display $ view monsterCard space in
    let pos     = Position.display $ view monsterPosition space in
    [i|#{pl} switched #{monster} (at #{location}) to #{pos}|]

  Turn turn player1 player2 ->
    let pl1   = view Player.name       player1 in
    let pl2   = view Player.name       player2 in
    let pl1lp = view Player.lifePoints player1 in
    let pl2lp = view Player.lifePoints player2 in
    [i|TURN #{turn} (#{pl1}: #{pl1lp} LP) (#{pl2}: #{pl2lp} LP)|]
