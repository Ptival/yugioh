{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module YuGiOh.Fresh
  ( Counters (..),
    Fresh,
    freshInt,
    freshString,
    handleFresh,
  )
where

import Control.Lens
import Polysemy
import Polysemy.State

data Fresh (m :: * -> *) a where
  FreshInt :: Fresh m Int
  FreshString :: String -> Fresh m String

makeSem ''Fresh

data Counters
  = Counters
      { _countInts :: Int,
        _countStrings :: Int
      }

makeLenses ''Counters

runFresh ::
  Member (State Counters) e =>
  Fresh m a ->
  Sem e a
runFresh FreshInt =
  do
    i <- gets (view countInts)
    modify (over countInts (+ 1))
    return i
runFresh (FreshString prefix) =
  do
    i <- gets (view countStrings)
    modify (over countStrings (+ 1))
    return $ prefix ++ show i

handleFresh ::
  Sem (Fresh ': e) a ->
  Sem (State Counters ': e) a
handleFresh = reinterpret runFresh
