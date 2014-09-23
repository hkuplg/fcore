module StringUtils
  ( capitalize
  ) where

import qualified Data.Char as Char (toUpper)

capitalize :: String -> String
capitalize "" = ""
capitalize (s:ss) = Char.toUpper s : ss
