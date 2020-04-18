module YuGiOh.Card.CurseOfDragon
  ( curseOfDragon,
  )
where

import YuGiOh.Attribute
import YuGiOh.Card
import YuGiOh.Type

curseOfDragon :: Card
curseOfDragon =
  Card
    { _name = "Curse of Dragon",
      _attribute = Dark,
      _level = 5,
      _monsterType = Dragon,
      _description = "A wicked dragon that taps into dark forces to execute a powerful attack.",
      _attack = 2000,
      _defense = 1500
    }
