module Card.CurseOfDragon (
  curseOfDragon,
  ) where

import Attribute
import Card
import Type

curseOfDragon :: MonsterCard
curseOfDragon = Monster
  { _monsterName = "Curse of Dragon"
  , _attribute   = Dark
  , _level       = 5
  , _monsterType = Dragon
  , _description = "A wicked dragon that taps into dark forces to execute a powerful attack."
  , _attack      = 2000
  , _defense     = 1500
  }
