{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Define the `Player` datatype.

module Player (
  Player,
  Player.display,
  hand,
  lifePoints,
  makePlayer,
  mat,
  Player.name,
  ) where

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Lens
import Data.String.Interpolate
import GHC.Generics

import Card
import Configuration
import Duelist
import Mat
import Utils

data Player = Player
  { _name       :: String
  , _hand       :: [Card.Card]
  , _lifePoints :: Int
  , _mat        :: Mat.Mat
  }
  deriving (Eq, Generic, Show)

makeLenses ''Player

makePlayer ::
  ( Member (Reader Configuration) e ) =>
  Duelist -> Eff e Player
makePlayer duelist = do
  lp <- view originalLifePoints <$> ask
  playerMat <- Mat.makeMat $ view Duelist.deck duelist
  return $ Player
    { _name       = view Duelist.name duelist
    , _hand       = []
    , _lifePoints = lp
    , _mat        = playerMat
    }

display :: Player -> String
display (Player {..}) =
  let h = displayList Card.display _hand in
  [i|#{_name} (LP: #{_lifePoints})
Hand (#{length _hand}):
#{h}
TEST
#{Mat.display _mat}|]
