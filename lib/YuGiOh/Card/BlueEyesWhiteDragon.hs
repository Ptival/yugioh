-- | An iconic card, the Blue Eyes White Dragon.
module YuGiOh.Card.BlueEyesWhiteDragon
  ( blueEyesWhiteDragon,
  )
where

import YuGiOh.Attribute
import YuGiOh.Card
import YuGiOh.Type

blueEyesWhiteDragon :: Card
blueEyesWhiteDragon =
  Card
    { _name = "Blue Eyes White Dragon",
      _attribute = Light,
      _level = 8,
      _monsterType = Dragon,
      _description = "This legendary dragon is a powerful engine of destruction. Virtually invincible, very few have faced this awesome creature and lived to tell the tale.",
      _attack = 3000,
      _defense = 2500
    }
