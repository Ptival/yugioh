-- | An iconic card, the Blue Eyes White Dragon.

module Card.BlueEyesWhiteDragon (
  blueEyesWhiteDragon,
  ) where

import Attribute
import Card
import Type

blueEyesWhiteDragon :: MonsterCard
blueEyesWhiteDragon = Monster
  { _monsterName = "Blue Eyes White Dragon"
  , _attribute   = Light
  , _level       = 8
  , _monsterType = Dragon
  , _description = "This legendary dragon is a powerful engine of destruction. Virtually invincible, very few have faced this awesome creature and lived to tell the tale."
  , _attack      = 3000
  , _defense     = 2500
  }
