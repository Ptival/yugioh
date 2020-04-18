{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A `Log` is a list of `Entry` entries that describe what happens through a
-- | duel.
module YuGiOh.Log
  ( Entry (..),
    YuGiOh.Log.Log,
  )
where

import Control.Lens
import Data.String.Interpolate
import YuGiOh.Card as Card
import YuGiOh.Classes.Displayable
import YuGiOh.Player as Player
import YuGiOh.Space

type Log = [Entry]

data Entry
  = Attacked Player MonsterSpace Player MonsterSpace
  | DamageInflicted Player Int
  | Destroyed Player MonsterSpace
  | DirectAttacked Player MonsterSpace Player
  | DrewCard Player Card
  | EndBattlePhase
  | EndDrawPhase
  | EndMainPhase
  | Flipped Player MonsterSpace
  | NormalSummoned Player MonsterSpace Int
  | TributeSummoned Player MonsterSpace Int
  | SentToGraveyard Player Card
  | SwitchedPosition Player MonsterSpace Int
  | Turn Int Player Player

instance Displayable Entry where
  display (Attacked sourcePlayer sourceMonster targetPlayer targetMonster) =
    let sp = view Player.name sourcePlayer
        sm = display sourceMonster
        tp = view Player.name targetPlayer
        tm = display targetMonster
     in [i|#{sp}'s #{sm} attacked #{tp}'s #{tm}|]
  display (DamageInflicted player damage) =
    [i|#{view Player.name player} receives #{show damage} damage|]
  display (Destroyed player monster) =
    [i|#{view Player.name player}'s #{display monster} got destroyed|]
  display (DirectAttacked sourcePlayer sourceMonster targetPlayer) =
    let sp = view Player.name sourcePlayer
        sm = display sourceMonster
        tp = view Player.name targetPlayer
     in [i|#{sp}'s #{sm} attacked #{tp} directly|]
  display (DrewCard player card) =
    [i|#{view Player.name player} drew #{display card}|]
  display EndBattlePhase = "Battle phase ended"
  display EndDrawPhase = "Draw phase ended"
  display EndMainPhase = "Main phase ended"
  display (Flipped player monster) =
    let pl = view Player.name player
        m = display $ view monsterCard monster
     in [i|#{pl}'s #{m} got flipped|]
  display (NormalSummoned player monster location) =
    let pl = view Player.name player
        m = display $ view monsterCard monster
        pos = display $ view monsterPosition monster
     in [i|#{pl} normal summoned #{m} (at #{location}) in #{pos}|]
  display (TributeSummoned player monster location) =
    let pl = view Player.name player
        m = display $ view monsterCard monster
        pos = display $ view monsterPosition monster
     in [i|#{pl} tribute summoned #{m} (at #{location}) in #{pos}|]
  display (SentToGraveyard player card) =
    let pl = view Player.name player
        c = display card
     in [i|#{pl}'s #{c} was sent to the graveyard|]
  display (SwitchedPosition player space location) =
    let pl = view Player.name player
        monster = display $ view monsterCard space
        pos = display $ view monsterPosition space
     in [i|#{pl} switched #{monster} (at #{location}) to #{pos}|]
  display (Turn turn player1 player2) =
    let pl1 = view Player.name player1
        pl2 = view Player.name player2
        pl1lp = view Player.lifePoints player1
        pl2lp = view Player.lifePoints player2
     in [i|TURN #{turn} (#{pl1}: #{pl1lp} LP) (#{pl2}: #{pl2lp} LP)|]
