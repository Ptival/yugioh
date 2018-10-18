{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | We define our own version of a `Zipper`, where the value under cursor can
-- | have a different (but compatible) type from the values out of cursor.  This
-- | lets us keep track of more precise static information using GADTs.

module Zipper (
  Zipper(..),
  allSelectZippers,
  allZippers,
  beforeCursor,
  cursor,
  cursorIndex,
  toList,
  toListWithCursorDeleted,
  zipperAtIndex,
  ) where

import Control.Lens

data Zipper cursor others = Zipper
  { _beforeCursor :: [others]
  , _cursor       :: cursor
  , _cursorIndex  :: Int
  , _afterCursor  :: [others]
  }

makeLenses ''Zipper

allZippers :: [a] -> [Zipper a a]
allZippers []    = []
allZippers (h:t) =
  let updateZipper
        = over beforeCursor (h :)
        . over cursorIndex (1 +)
  in
  Zipper [] h 0 t : map updateZipper (allZippers t)

allSelectZippers ::
  (others -> [others] -> [Zipper cursor others]) -> [others] -> [Zipper cursor others]
allSelectZippers addZipper = \case
  [] -> []
  h:t ->
    let updateZipper
          = over beforeCursor (h :)
          . over cursorIndex (1 +)
    in
    addZipper h t ++ map updateZipper (allSelectZippers addZipper t)

-- | Returns the original list, minus the element under focus
toListWithCursorDeleted :: Zipper cursor others -> [others]
toListWithCursorDeleted z = view beforeCursor z ++ view afterCursor z

toList :: (cursor -> a) -> (others -> a) -> Zipper cursor others -> [a]
toList fCursor fOthers z =
  (map fOthers $ view beforeCursor z)
  ++ [fCursor $ view cursor z]
  ++ (map fOthers $ view afterCursor z)

zipperAtIndex :: Int -> [a] -> Maybe (Zipper a a)
zipperAtIndex _cursorIndex list =
  case splitAt _cursorIndex list of
  (_beforeCursor, _cursor : _afterCursor) ->
    Just $ Zipper
    { _beforeCursor
    , _cursor
    , _cursorIndex
    , _afterCursor
    }
  _ -> Nothing
