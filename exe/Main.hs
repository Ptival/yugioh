{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Run our simulator!
module Main
  ( main,
  )
where

import Control.Monad (forM_)
import Polysemy
import Polysemy.Fail
import Polysemy.State
import YuGiOh.Card.BeaverWarrior
import YuGiOh.Card.BlueEyesWhiteDragon
import YuGiOh.Card.CurseOfDragon
import YuGiOh.Card.DarkMagician
import YuGiOh.Card.SaggiTheDarkClown
import YuGiOh.ChooseOption
import YuGiOh.Classes.Displayable
import YuGiOh.Driver
import YuGiOh.Duelist
import YuGiOh.Fresh
import YuGiOh.Utils

setoKaiba :: Duelist
setoKaiba =
  Duelist
    { _name = "Seto Kaiba",
      _deck =
        []
          ++ replicate 3 blueEyesWhiteDragon
          ++ replicate 3 saggiTheDarkClown
    }

yamiYugi :: Duelist
yamiYugi =
  Duelist
    { _name = "Yami Yugi",
      _deck =
        []
          ++ replicate 3 beaverWarrior
          ++ replicate 3 curseOfDragon
          ++ replicate 3 darkMagician
    }

data ChooseOptionHandler
  = Manually
  | Randomly

getChooseOptionHandler ::
  Member (Embed IO) e =>
  ChooseOptionHandler ->
  (forall a. ChooseOption m a -> Sem e a)
getChooseOptionHandler = \case
  Manually -> handleChooseOptionIO
  Randomly -> handleChooseOptionRandom

main :: IO ()
main = do
  putStrLn "IT'S TIME... TO D-D-D-DUEL!"
  let prompt = "How do you wish to play the game?"
  let options =
        [ ("Manually (you pick moves for both players)", Manually),
          ("Randomly (computer picks random moves for both players)", Randomly)
        ]
  chooseMoveHandlerDescriptor <- promptForOption prompt options
  Right (duelLog, victory) <-
    runFinal . embedToFinal @IO
      $ handleChooseOption (getChooseOptionHandler chooseMoveHandlerDescriptor)
      $ runFail
      $ (snd <$>) $ runState (Counters 0 0)
      $ handleFresh
      $ runDuel setoKaiba yamiYugi
  forM_ duelLog (putStrLn . display)
  putStrLn $ display victory
