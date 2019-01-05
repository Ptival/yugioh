-- | Yugi's companion.

module Card.DarkMagician (
  darkMagician,
  ) where

import Attribute
import Card
import Type

darkMagician :: MonsterCard
darkMagician = Monster
  { _monsterName = "Dark Magician"
  , _attribute   = Dark
  , _level       = 7
  , _monsterType = Spellcaster
  , _description = "The ultimate wizard in terms of attack and defense."
  , _attack      = 2500
  , _defense     = 2100
  }
