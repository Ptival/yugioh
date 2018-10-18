{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

-- | `ChooseMove` is an effect for choosing a move within a list of valid moves,

module ChooseMove (
  ChooseMove,
  chooseMove,
  handleChooseMoveIO,
  handleChooseMoveRandom,
  runChooseMoveIO,
  ) where

import           Control.Eff             (Member)
import           Control.Eff.Extend      (Eff(..), (^$), decomp, send)
import           Control.Exception       (Exception, throwIO)
import           Control.Lens            (view)
import           Control.Monad           (when)
import           Data.String.Interpolate (i)
import           System.Random           (randomRIO)

import           Card
import           Duel
import qualified Lenses                  as L
import           Mat
import           Move
import           Utils

data ChooseMove a where
  ChooseMove :: Duel -> [Move p] -> ChooseMove (Move p)

chooseMove :: ( Member ChooseMove e ) => Duel -> [Move p] -> Eff e (Move p)
chooseMove duel validMoves = send $ ChooseMove duel validMoves

data Impossible = Impossible deriving (Show)
instance Exception Impossible

runChooseMoveIO :: (forall b. ChooseMove b -> IO b) -> Eff '[ChooseMove] a -> IO a
runChooseMoveIO _           (Val x) = return x
runChooseMoveIO howToHandle (E u q) = case decomp u of
     Right command -> do
       chosenMove <- howToHandle command
       runChooseMoveIO howToHandle (q ^$ chosenMove)
     Left _ -> throwIO Impossible

handleChooseMoveIO :: ChooseMove a -> IO a
handleChooseMoveIO (ChooseMove duel validMoves) = do
  let currentPlayerHand = view L.currentPlayerHand duel
  let currentPlayerMat  = view L.currentPlayerMat  duel
  let otherPlayerMat    = view L.otherPlayerMat    duel
  let prompt            = [i|
Other player's mat:
#{Mat.display (DisplayDeck False) otherPlayerMat}
Current player's mat:
#{Mat.display (DisplayDeck False) currentPlayerMat}
Current player's hand:
#{displayList Card.display currentPlayerHand}
Choose a move among the following ones.
|]
  let options           = mapWithIndex (\ index move -> (Move.display move, index)) validMoves
  moveIndex <- promptForOption prompt options
  let chosenMove = validMoves !! moveIndex
  putStrLn [i|Chosen move: #{Move.display chosenMove}|]
  return chosenMove

handleChooseMoveRandom :: ChooseMove a -> IO a
handleChooseMoveRandom (ChooseMove _ validMoves) = do
  let len = length validMoves
  when (len == 0) $ throwIO EmptyList
  moveIndex <- randomRIO (0, len - 1)
  let chosenMove = validMoves !! moveIndex
  return chosenMove
