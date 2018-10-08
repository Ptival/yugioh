{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

-- |

module Utils (
  (<*^>),
  askLensed,
  displayList,
  getLensed,
  Utils.log,
  overLensed,
  setLensed,
  ) where

import Control.Eff
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Eff.Writer.Strict
import Control.Lens
import Data.List

infixl 4 <*^>

(<*^>) :: Applicative f => f (a -> b) -> a -> f b
(<*^>) f v = f <*> pure v

displayList :: (a -> String) -> [a] -> String
displayList displayElement = intercalate "\n" . map displayElement

askLensed :: Member (Reader s) r => Getting b s b -> Eff r b
askLensed l = view l <$> ask

getLensed :: Member (State s) r => Getting b s b -> Eff r b
getLensed l = view l <$> get

log :: Member (Writer [a]) r => a -> Eff r ()
log v = tell [v]

overLensed :: Member (State s) r => ASetter s s a b -> (a -> b) -> Eff r ()
overLensed l f = modify (over l f)

setLensed :: Member (State s) r => ASetter s s a b -> b -> Eff r ()
setLensed l v = modify (set l v)
