module YuGiOh.Classes.Displayable
  ( Displayable (..),
  )
where

class Displayable t where
  display :: t -> String
