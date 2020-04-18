-- | One of Yugi's first cards, the Beaver Warrior.
module YuGiOh.Card.BeaverWarrior
  ( beaverWarrior,
  )
where

import YuGiOh.Attribute
import YuGiOh.Card
import YuGiOh.Type

beaverWarrior :: Card
beaverWarrior =
  Card
    { _name = "Beaver Warrior",
      _attribute = Earth,
      _level = 4,
      _monsterType = BeastWarrior,
      _description = "What this creature lacks in size it makes up for in defense when battling in the prairie.",
      _attack = 1200,
      _defense = 1500
    }
