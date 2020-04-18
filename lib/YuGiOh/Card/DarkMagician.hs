-- | Yugi's companion.
module YuGiOh.Card.DarkMagician
  ( darkMagician,
  )
where

import YuGiOh.Attribute
import YuGiOh.Card
import YuGiOh.Type

darkMagician :: Card
darkMagician =
  Card
    { _name = "Dark Magician",
      _attribute = Dark,
      _level = 7,
      _monsterType = Spellcaster,
      _description = "The ultimate wizard in terms of attack and defense.",
      _attack = 2500,
      _defense = 2100
    }
