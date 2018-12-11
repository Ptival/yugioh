{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | Run our simulator!

module Main (
  main,
  ) where

import Control.Monad (forM_)

import Card.BeaverWarrior
import Card.BlueEyesWhiteDragon
import Card.DarkMagician
import Card.SaggiTheDarkClown
import ChooseMove
import Driver
import Duelist
import Log
import Utils
import Victory

setoKaiba :: Duelist
setoKaiba = Duelist
  { _name = "Seto Kaiba"
  , _deck = []
            ++ replicate 3 blueEyesWhiteDragon
            ++ replicate 3 saggiTheDarkClown
  }

yamiYugi :: Duelist
yamiYugi = Duelist
  { _name = "Yami Yugi"
  , _deck = []
            ++ replicate 3 beaverWarrior
            ++ replicate 3 darkMagician
  }

data ChooseMoveHandler
  = Manually
  | Randomly

getChooseMoveHandler :: ChooseMoveHandler -> (forall a. ChooseMove a -> IO a)
getChooseMoveHandler = \case
  Manually -> handleChooseMoveIO
  Randomly -> handleChooseMoveRandom

main :: IO ()
main = do
  putStrLn "IT'S TIME... TO D-D-D-DUEL!"
  let prompt = "How do you wish to play the game?"
  let options =
        [ ("Manually (you pick moves for both players)",              Manually)
        , ("Randomly (computer picks random moves for both players)", Randomly)
        ]
  chooseMoveHandlerDescriptor <- promptForOption prompt options
  let chooseMoveHandler = getChooseMoveHandler chooseMoveHandlerDescriptor
  (victory, duelLog) <- runChooseMoveIO chooseMoveHandler $ runDuel setoKaiba yamiYugi
  forM_ duelLog (putStrLn . Log.display)
  putStrLn $ Victory.display victory
