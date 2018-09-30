-- | Run our simulator!

module Main (
  main,
  ) where

import Card
import Card.BlueEyesWhiteDragon

main :: IO ()
main = do
  putStrLn $ display blueEyesWhiteDragon
