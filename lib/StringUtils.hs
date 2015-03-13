{- |
Module      :  StringUtils
Description :  Utilities for string operations.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module StringUtils
  ( capitalize
  ) where

import qualified Data.Char as Char (toUpper)

capitalize :: String -> String
capitalize "" = ""
capitalize (s:ss) = Char.toUpper s : ss
