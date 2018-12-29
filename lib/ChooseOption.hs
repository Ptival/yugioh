{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | `ChooseOption` is an effect for choosing an option among several.

module ChooseOption (
  ChooseOption,
  chooseOption,
  handleChooseOptionIO,
  handleChooseOptionRandom,
  handleChooseOption,
  ) where

import           Control.Eff             (Lifted, Member, lift)
import           Control.Eff.Extend      (Eff(..), (^$), decomp, qComps, send)
import           Control.Exception       (Exception, throwIO)
import           Control.Lens            (view)
import           Control.Monad           (when)
import           Data.String.Interpolate (i)
import           System.Random           (randomRIO)

import           Card
import           Duel
import qualified Lenses                  as L
import           Mat
import           Utils

data ChooseOption a where
  ChooseOption :: forall option. Duel -> (option -> String) -> [option] -> ChooseOption option

chooseOption ::
  ( Member ChooseOption e ) =>
  Duel -> (option -> String) -> [option] -> Eff e option
chooseOption duel displayOption validOptions =
  send $ ChooseOption duel displayOption validOptions

data Impossible = Impossible deriving (Show)
instance Exception Impossible

handleChooseOption ::
  Lifted IO e =>
  (forall b. ChooseOption b -> Eff e b) -> Eff (ChooseOption ': e) a -> Eff e a
handleChooseOption _           (Val x) = return x
handleChooseOption howToHandle (E u q) = case decomp q of
     Right command -> do
       chosenOption <- howToHandle command
       handleChooseOption howToHandle (u ^$ chosenOption)
     Left u1 -> E (qComps u (handleChooseOption howToHandle)) u1

handleChooseOptionIO ::
  Lifted IO e =>
  ChooseOption option -> Eff e option
handleChooseOptionIO (ChooseOption duel displayOption validOptions) = lift $ do
  let currentPlayerHand = view (L.hand L.currentPlayer) duel
  let currentPlayerMat  = view (L.mat  L.currentPlayer) duel
  let otherPlayerMat    = view (L.mat  L.otherPlayer)   duel
  let prompt            = [i|
Other player's mat:
#{Mat.display (DisplayDeck False) otherPlayerMat}
Current player's mat:
#{Mat.display (DisplayDeck False) currentPlayerMat}
Current player's hand:
#{displayList Card.display currentPlayerHand}
Choose an option among the following ones.
|]
  let options           = mapWithIndex (\ index option -> (displayOption option, index)) validOptions
  optionIndex <- promptForOption prompt options
  let chosenOption = validOptions !! optionIndex
  putStrLn [i|Chosen option: #{displayOption chosenOption}|]
  return chosenOption

handleChooseOptionRandom ::
  Lifted IO e =>
  ChooseOption option -> Eff e option
handleChooseOptionRandom (ChooseOption _ _ validOptions) = lift $ do
  let len = length validOptions
  when (len == 0) $ throwIO EmptyList
  optionIndex <- randomRIO (0, len - 1)
  let chosenOption = validOptions !! optionIndex
  return chosenOption
