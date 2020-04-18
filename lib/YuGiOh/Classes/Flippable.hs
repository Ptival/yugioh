module YuGiOh.Classes.Flippable
  ( Flippable (..),
  )
where

-- | @Flippable@ captures those data types that understand the @flip@ operation.
class Flippable t where
  flip :: t -> t
