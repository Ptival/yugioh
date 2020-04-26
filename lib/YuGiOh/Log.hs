{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A `Log` is a list of `Entry` entries that describe what happens through a
-- | duel.
module YuGiOh.Log
  ( Entry (..),
    Log,
  )
where

import Control.Lens
import Data.String.Interpolate
import YuGiOh.Card as Card
import YuGiOh.Classes.Displayable
import YuGiOh.Phase
import YuGiOh.Player as Player
import YuGiOh.Space

type Log = [Entry]

data Entry
  = Attacked Player MonsterSpace Player MonsterSpace
  | DamageInflicted Player Int
  | Destroyed Player MonsterSpace
  | DirectAttacked Player MonsterSpace Player
  | DrewCard Player Card
  | Ended Phase
  | Entered Phase
  | Flipped Player MonsterSpace
  | NormalSummoned Player MonsterSpace Int
  | TributeSummoned Player MonsterSpace Int
  | SentToGraveyard Player Card
  | SwitchedPosition Player MonsterSpace Int
  | Turn Int Player Player

playersNames :: (Player, Player) -> (String, String)
playersNames (p1, p2) = (view Player.name p1, view Player.name p2)

data SummonType
  = NormalSummon
  | TributeSummon

summoned :: Player -> SummonType -> MonsterSpace -> Int -> String
summoned player summonType monster location =
  let pl = view Player.name player
      st = case summonType of
        NormalSummon -> "normal"
        TributeSummon -> "tribute"
      m = display $ view monsterCard monster
      pos = display $ view monsterPosition monster
  in [i|#{pl} #{st} summoned #{m} (at #{location}) in #{pos}|]

instance Displayable Entry where
  display (Attacked sourcePlayer sourceMonster targetPlayer targetMonster) =
    let (sp, tp) = playersNames (sourcePlayer, targetPlayer)
        sm = display sourceMonster
        tm = display targetMonster
     in [i|#{sp}'s #{sm} attacked #{tp}'s #{tm}|]
  display (DamageInflicted player damage) =
    [i|#{view Player.name player} receives #{show damage} damage|]
  display (Destroyed player monster) =
    [i|#{view Player.name player}'s #{display monster} got destroyed|]
  display (DirectAttacked sourcePlayer sourceMonster targetPlayer) =
    let (sp, tp) = playersNames (sourcePlayer, targetPlayer)
        sm = display sourceMonster
     in [i|#{sp}'s #{sm} attacked #{tp} directly|]
  display (DrewCard player card) =
    [i|#{view Player.name player} drew #{display card}|]
  display (Ended phase) = "Ended " ++ display phase
  display (Entered phase) = "Entered " ++ display phase
  display (Flipped player monster) =
    let pl = view Player.name player
        m = display $ view monsterCard monster
     in [i|#{pl}'s #{m} got flipped|]
  display (NormalSummoned player monster location) =
    summoned player NormalSummon monster location
  display (TributeSummoned player monster location) =
    summoned player TributeSummon monster location
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
     in [i|TURN #{turn} (Turn player: #{pl1} #{pl1lp} LP) (Other player: #{pl2} #{pl2lp} LP)|]
