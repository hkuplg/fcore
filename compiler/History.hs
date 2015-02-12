{- |
Module      :  History
Description :  REPL command history
Copyright   :  (c) 2014-2015 HKU
License     :  BSD3

Maintainer  :  u3502350@connect.hku.hk
Stability   :  stable
Portability :  portable
-}

module History where
-- for :replay

type Hist = [String]

empty :: Hist
empty = []

insert :: Hist -> String -> Hist 
insert xs cmd = cmd : xs


