{- |
Module      :  ParseCMD
Description :  Parser for the REPL
Copyright   :  (c) 2014-2015 HKU
License     :  BSD3

Maintainer  :  Boya Peng <u3502350@connect.hku.hk>
Stability   :  stable
Portability :  portable

-}

module ParseCMD where

import Text.ParserCombinators.Parsec

line = sepBy cell (char ' ')
cell = many (noneOf " ")

parseMsg :: String -> Either ParseError [String]
parseMsg input = parse line "(unknown)" input


