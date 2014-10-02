module ParseCMD where

import Text.ParserCombinators.Parsec

line = sepBy cell (char ' ')
cell = many (noneOf " ")

parseMsg :: String -> Either ParseError [String]
parseMsg input = parse line "(unknown)" input


