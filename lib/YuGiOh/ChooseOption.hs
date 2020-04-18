{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | `ChooseOption` is an effect for choosing an option among several.
module YuGiOh.ChooseOption
  ( ChooseOption,
    chooseOption,
    handleChooseOptionIO,
    handleChooseOptionRandom,
    handleChooseOption,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Lens (view)
import Control.Monad (when)
import Data.String.Interpolate (i)
import Polysemy (Member, Sem, interpret, makeSem)
import Polysemy.Embed
import System.Random (randomRIO)
import YuGiOh.Classes.Displayable
import YuGiOh.Duel
import qualified YuGiOh.Lenses as L
import YuGiOh.Utils

data ChooseOption (m :: * -> *) option where
  ChooseOption :: Duel -> (option -> String) -> [option] -> ChooseOption m option

makeSem ''ChooseOption

data Impossible = Impossible deriving (Show)

instance Exception Impossible

handleChooseOption ::
  Member (Embed IO) e =>
  (forall m b. ChooseOption m b -> Sem e b) ->
  Sem (ChooseOption ': e) a ->
  Sem e a
handleChooseOption = interpret

handleChooseOptionIO ::
  Member (Embed IO) e =>
  ChooseOption m option ->
  Sem e option
handleChooseOptionIO (ChooseOption duel displayOption validOptions) = embed $ do
  let currentPlayerHand = view (L.hand L.currentPlayer) duel
  let currentPlayerMat = view (L.mat L.currentPlayer) duel
  let otherPlayerMat = view (L.mat L.otherPlayer) duel
  let prompt =
        [i|
Other player's mat:
#{display otherPlayerMat}
Current player's mat:
#{display currentPlayerMat}
Current player's hand:
#{displayList display currentPlayerHand}
Choose an option among the following ones.
|]
  let options = mapWithIndex (\index option -> (displayOption option, index)) validOptions
  optionIndex <- promptForOption prompt options
  let chosenOption = validOptions !! optionIndex
  putStrLn [i|Chosen option: #{displayOption chosenOption}|]
  return chosenOption

handleChooseOptionRandom ::
  Member (Embed IO) e =>
  ChooseOption m option ->
  Sem e option
handleChooseOptionRandom (ChooseOption _ _ validOptions) = embed $ do
  let len = length validOptions
  when (len == 0) $ throwIO EmptyList
  optionIndex <- randomRIO (0, len - 1)
  let chosenOption = validOptions !! optionIndex
  return chosenOption
