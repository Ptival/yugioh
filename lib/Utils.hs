-- |

module Utils (
  displayList,
  ) where

import Data.List

displayList :: (a -> String) -> [a] -> String
displayList displayElement = intercalate "\n" . map displayElement
