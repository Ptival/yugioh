{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Define the `Player` datatype.

module Player (
  Player,
  display,
  hand,
  lifePoints,
  makePlayer,
  mat,
  name,
  ) where

import           Control.Eff
import           Control.Eff.Reader.Strict
import           Control.Lens
import           Data.String.Interpolate
import           GHC.Generics

import qualified Card
import           Configuration
import qualified Mat
import           Utils

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
  String -> [Card.Card] -> Eff e Player
makePlayer playerName playerDeck = do
  lp <- view originalLifePoints <$> ask
  playerMat <- Mat.makeMat playerDeck
  return $ Player
    { _name       = playerName
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
