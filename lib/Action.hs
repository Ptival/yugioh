{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Whatever where

import GHC.Exts
import GHC.TypeLits

data IsMember k
  = IsMember
  | IsNotMember k [k]

type family MemberB (x :: k) (l :: [k]) (orig :: [k]) where
  MemberB a '[]      orig = 'IsNotMember a orig
  MemberB a (a : _)  _    = 'IsMember
  MemberB a (b : xs) orig = MemberB a xs orig

type Member x xs = MemberB x xs xs ~ 'IsMember

type family Member' x l mem :: Constraint where
  Member' x l 'True = ()
  Member' x l 'False =
    TypeError ('ShowType x ':<>:
               'Text " is not a member of " ':<>:
               'ShowType l)

data Configuration = A | B | C

data Action (configuration :: Configuration) where
  Action1 :: Member cfg '[ 'A ]     => Action cfg
  Action2 :: Member cfg '[ 'B, 'C ] => Action cfg
  Action3 :: Member cfg '[ 'A, 'C ] => Action cfg

exhaustive :: Action 'A -> ()
exhaustive Action1 = ()
exhaustive Action2 = ()
exhaustive Action3 = ()
