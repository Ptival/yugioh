{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a type family for marking a data kind as being part of
-- | a given list.  This is useful to restrict certain constructors in certain
-- | contexts, by using GADTs.  This lets the exhaustiveness checker for pattern
-- | matching be smarter about unfeasible patterns based on type indices.

module In where

data IsIn k
  = IsIn
  | IsNotIn k [k]

type family InB (x :: k) (l :: [k]) (orig :: [k]) where
  InB x '[]      orig = 'IsNotIn x orig
  InB x (x : _)  _    = 'IsIn
  InB x (b : xs) orig = InB x xs orig

type In x l = InB x l l ~ 'IsIn
