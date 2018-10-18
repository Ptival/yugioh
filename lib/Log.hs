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
import Mat
import Player
import Position
import Space
import Zipper

type Log = [Entry]

data Entry
  = AddedCardToHand  Player Card
  | DrewCard         Player Card
  | EndDrawPhase
  | EndMainPhase
  | EndTurn
  | NormalSummoned   Player
                     (Zipper (Space 'IsMonsterCard) ScopedSpace) -- location on mat
  | SwitchedPosition Player
                     (Zipper (Space 'IsMonsterCard) ScopedSpace) -- new position

display :: Entry -> String
display = \case

  AddedCardToHand player card ->
    [i|#{view Player.name player} added card #{Card.display card} to their hand|]

  DrewCard player card ->
    [i|#{view Player.name player} drew card #{Card.display card}|]

  EndDrawPhase -> "Draw phase ended"

  EndMainPhase -> "Main phase ended"

  EndTurn -> "Turn ended"

  NormalSummoned player zipper ->
    let pl      = view Player.name player in
    let space   = view cursor zipper in
    let monster = Card.display $ view monsterCard space in
    let loc     = view cursorIndex zipper in
    let pos     = Position.display $ view monsterPosition space in
    [i|#{pl} normal summoned #{monster} (at #{loc}) in #{pos}|]

  SwitchedPosition player zipper ->
    let pl      = view Player.name player in
    let space   = view cursor zipper in
    let monster = Card.display $ view monsterCard space in
    let loc     = view cursorIndex zipper in
    let pos     = Position.display $ view monsterPosition space in
    [i|#{pl} switched #{monster} (at #{loc}) to #{pos}|]
