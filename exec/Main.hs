{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run our simulator!

module Main (
  main,
  ) where

import Control.Eff              (Eff, Lifted, runLift)
import Control.Monad            (forM_)

import Card
import Card.BeaverWarrior
import Card.BlueEyesWhiteDragon
import Card.CurseOfDragon
import Card.DarkMagician
import Card.SaggiTheDarkClown
import ChooseOption
import Driver
import Duelist
import Log
import Utils
import Victory

setoKaiba :: Duelist
setoKaiba = Duelist
  { _name = "Seto Kaiba"
  , _deck = []
            ++ replicate 3 (anyCard blueEyesWhiteDragon)
            ++ replicate 3 (anyCard saggiTheDarkClown)
  }

yamiYugi :: Duelist
yamiYugi = Duelist
  { _name = "Yami Yugi"
  , _deck = []
            ++ replicate 3 (anyCard beaverWarrior)
            ++ replicate 3 (anyCard curseOfDragon)
            ++ replicate 3 (anyCard darkMagician)
  }

data ChooseOptionHandler
  = Manually
  | Randomly

getChooseOptionHandler ::
  Lifted IO e =>
  ChooseOptionHandler -> (forall a. ChooseOption a -> Eff e a)
getChooseOptionHandler = \case
  Manually -> handleChooseOptionIO
  Randomly -> handleChooseOptionRandom

main :: IO ()
main = do
  putStrLn "IT'S TIME... TO D-D-D-DUEL!"
  let prompt = "How do you wish to play the game?"
  let options =
        [ ("Manually (you pick moves for both players)",              Manually)
        , ("Randomly (computer picks random moves for both players)", Randomly)
        ]
  chooseMoveHandlerDescriptor <- promptForOption prompt options
  (victory, duelLog) <- runLift
                        $ handleChooseOption (getChooseOptionHandler chooseMoveHandlerDescriptor)
                        $ runDuel setoKaiba yamiYugi
  forM_ duelLog (putStrLn . Log.display)
  putStrLn $ Victory.display victory
