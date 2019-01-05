{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

-- | This module contains general purpose functions that are not provided by
-- | other libraries, or only provided by libraries we do not desire to depend
-- | on.

module Utils (
  EmptyList(..),
  (<*^>),
  askLensed,
  compose,
  displayList,
  getALensed,
  getLensed,
  Utils.log,
  mapWithIndex,
  overLensed,
  promptForOption,
  setLensed,
  ) where

import Control.Eff               (Eff, Member)
import Control.Eff.Reader.Strict (Reader, ask)
import Control.Eff.State.Strict  (State, get, modify)
import Control.Eff.Writer.Strict (Writer, tell)
import Control.Exception         (Exception, throwIO)
import Control.Lens              (ALens', ASetter, Getting, cloneLens, over, set, view)
import Control.Monad             (when, foldM_)
import Control.Monad.Loops       (untilJust)
import Data.List                 (foldl', intercalate)
import Data.String.Interpolate   (i)
import Text.Read                 (readMaybe)

infixl 4 <*^>

(<*^>) :: Applicative f => f (a -> b) -> a -> f b
(<*^>) f v = f <*> pure v

compose :: [a -> a] -> a -> a
compose = flip (foldl' (flip id))

displayList :: (a -> String) -> [a] -> String
displayList displayElement = intercalate "\n" . map displayElement

askLensed :: Member (Reader s) r => Getting b s b -> Eff r b
askLensed l = view l <$> ask

getALensed :: Member (State s) r => ALens' s b -> Eff r b
getALensed l = view (cloneLens l) <$> get

getLensed :: Member (State s) r => Getting b s b -> Eff r b
getLensed l = view l <$> get

log :: Member (Writer [a]) r => a -> Eff r ()
log v = tell [v]

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f l = snd $ foldl' (\ (ndx, b) a -> (ndx + 1, f ndx a : b)) (0, []) l

overLensed :: Member (State s) r => ASetter s s a b -> (a -> b) -> Eff r ()
overLensed l f = modify (over l f)

data EmptyList = EmptyList deriving (Show)
instance Exception EmptyList

-- | Given a `prompt` to display, and a (non-empty) list of `options` to choose
-- | from, repeatedly ask the user to pick one of the options, and returns the
-- | index of the chosen one.
promptForOption :: String -> [(String, a)] -> IO a
promptForOption prompt options = do
  let len = length options
  when (len == 0) $ throwIO EmptyList
  -- We will want to pad all numbers to take as much space as the largest one
  let leftPad showable =
        let desiredLength = length (show len) in
        let string = show showable in
        replicate (desiredLength - length string) ' ' ++ string
  let loopBody :: Int -> String -> IO Int
      loopBody currentIndex currentOption = do
        putStrLn [i|* #{leftPad currentIndex}. #{currentOption}|]
        return $ currentIndex + 1
  untilJust $ do
    putStrLn prompt
    foldM_ loopBody 1 $ map fst options -- Humans prefer counting from 1
    readMaybe <$> getLine >>= \case
      Nothing -> do
        putStrLn "[ERROR] not a number, please try again"
        return Nothing
      Just humanNumber ->
        if humanNumber < 0 || humanNumber > len
          then do
          putStrLn "[ERROR] number out of range, please try again"
          return Nothing
          else do
          let computerNumber = humanNumber - 1 -- Computers prefer counting from 0
          return $ Just $ snd $ options !! computerNumber

setLensed :: Member (State s) r => ASetter s s a b -> b -> Eff r ()
setLensed l v = modify (set l v)
