{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A `Player` is a participant in a duel.  Players are created from `Duelist`,
-- | but have extra dynamic information related to the duel (like, their current
-- | life points).

module Player (
  Hand,
  Player,
  Player.display,
  hand,
  hasDrawnCard,
  hasNormalSummoned,
  inflictDamage,
  lifePoints,
  makePlayer,
  mat,
  Player.name,
  Player.prepareForNewTurn,
  ) where

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Lens
import Data.String.Interpolate
import Card
import Configuration
import Duelist
import Mat
import Utils

type Hand = [Card.Card]

data Player = Player
  { _name              :: String
  , _hand              :: Hand
  , _hasDrawnCard      :: Bool
  , _hasNormalSummoned :: Bool
  , _lifePoints        :: Int
  , _mat               :: Mat.Mat
  }

makeLenses ''Player

makePlayer ::
  ( Member (Reader Configuration) e ) =>
  Duelist -> Eff e Player
makePlayer duelist = do
  lp <- view originalLifePoints <$> ask
  playerMat <- Mat.makeMat $ view Duelist.deck duelist
  return $ Player
    { _name              = view Duelist.name duelist
    , _hand              = []
    , _hasDrawnCard      = False
    , _hasNormalSummoned = False
    , _lifePoints        = lp
    , _mat               = playerMat
    }

prepareForNewTurn :: Player -> Player
prepareForNewTurn
  = set  hasDrawnCard      False
  . set  hasNormalSummoned False
  . over mat               Mat.prepareForNewTurn

display :: DisplayDeck -> Player -> String
display displayDeck (Player {..}) =
  let h = displayList Card.display _hand in
  [i|#{_name} (LP: #{_lifePoints})
Hand (#{length _hand}):
#{h}
TEST
#{Mat.display displayDeck _mat}|]

inflictDamage :: Int -> Player -> Player
inflictDamage damage = over lifePoints (\ lp -> max 0 (lp - damage))
