-- | A somewhat weak card for poor Seto Kaiba...
module YuGiOh.Card.SaggiTheDarkClown
  ( saggiTheDarkClown,
  )
where

import YuGiOh.Attribute
import YuGiOh.Card
import YuGiOh.Type

saggiTheDarkClown :: Card
saggiTheDarkClown =
  Card
    { _name = "Saggi the Dark Clown",
      _attribute = Dark,
      _level = 3,
      _monsterType = Spellcaster,
      _description = "This clown appears from nowhere and executes very strange moves to avoid enemy attacks.",
      _attack = 600,
      _defense = 1500
    }
