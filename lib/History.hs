{- |
Module      :  History
Description :  REPL command history
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Boya Peng <u3502350@connect.hku.hk>
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


