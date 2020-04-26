{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Player` is a participant in a duel.  Players are created from `Duelist`,
-- but have extra dynamic information related to the duel (like, their current
-- life points).
module YuGiOh.Player
  ( Hand,
    Player,
    hand,
    hasDrawnCard,
    hasNormalSummoned,
    identifier,
    inflictDamage,
    isSamePlayer,
    lifePoints,
    makePlayer,
    mat,
    YuGiOh.Player.name,
    YuGiOh.Player.prepareForNewTurn,
  )
where

import Control.Lens (makeLenses, over, set, view)
import Data.Function (on)
import Data.String.Interpolate (i)
import Polysemy (Member, Sem)
import Polysemy.Reader (Reader, ask)
import YuGiOh.Card as Card
import YuGiOh.Classes.Displayable
import YuGiOh.Configuration
import YuGiOh.Duelist as Duelist
import YuGiOh.Fresh
import YuGiOh.Mat as Mat
import YuGiOh.Utils

type Hand = [Card.Card]

data Player
  = Player
      { _name :: String,
        _hand :: Hand,
        _hasDrawnCard :: Bool,
        _hasNormalSummoned :: Bool,
        _identifier :: Int,
        _lifePoints :: Int,
        _mat :: Mat.Mat
      }

makeLenses ''Player

makePlayer ::
  Member Fresh e =>
  Member (Reader Configuration) e =>
  Duelist ->
  Sem e Player
makePlayer duelist = do
  ident <- freshInt
  lp <- view originalLifePoints <$> ask
  playerMat <- Mat.makeMat $ view Duelist.deck duelist
  return $
    Player
      { _name = view Duelist.name duelist,
        _hand = [],
        _hasDrawnCard = False,
        _hasNormalSummoned = False,
        _identifier = ident,
        _lifePoints = lp,
        _mat = playerMat
      }

prepareForNewTurn :: Player -> Player
prepareForNewTurn =
  set hasDrawnCard False
    . set hasNormalSummoned False
    . over mat Mat.prepareForNewTurn

instance Displayable Player where
  display Player {..} =
    let h = displayList display _hand
     in [i|#{_name} (LP: #{_lifePoints})
Hand (#{length _hand}):
 #{h}
TEST
 #{display _mat}|]

inflictDamage :: Int -> Player -> Player
inflictDamage damage = over lifePoints (\lp -> max 0 (lp - damage))

isSamePlayer :: Player -> Player -> Bool
isSamePlayer = (==) `on` view identifier
