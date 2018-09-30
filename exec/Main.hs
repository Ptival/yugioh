-- | Run our simulator!

module Main (
  main,
  ) where

import Control.Eff
import Control.Eff.Reader.Strict

import Card.BlueEyesWhiteDragon
import Configuration
import Player

player :: Player
player =
  run
  $ runReader testingConfiguration
  $ makePlayer "Seto Kaiba" [blueEyesWhiteDragon, blueEyesWhiteDragon]

main :: IO ()
main = do
  putStrLn $ display player
