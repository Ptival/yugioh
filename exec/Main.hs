-- | Run our simulator!

module Main (
  main,
  ) where

import Control.Monad

import Card.BeaverWarrior
import Card.BlueEyesWhiteDragon
import Duel
import Duelist
import Log
import Victory

setoKaiba :: Duelist
setoKaiba = Duelist
  { _name = "Seto Kaiba"
  , _deck = replicate 3 blueEyesWhiteDragon
  }

yamiYugi :: Duelist
yamiYugi = Duelist
  { _name = "Yami Yugi"
  , _deck = replicate 3 beaverWarrior
  }

main :: IO ()
main = do
  let ((victory, duelLog), _) = runDuel setoKaiba yamiYugi
  forM_ duelLog (putStrLn . Log.display)
  putStrLn $ Victory.display victory
