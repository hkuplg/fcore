{- |
Module      :  Z3ModelParser
Description :  A parser for parsing Z3 model format
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Weixin Zhang <zhangweixinxd@gmail.com>
Stability   :  unstable
Portability :  portable
-}

module Z3ModelParser where

import Text.ParserCombinators.Parsec
import Control.Monad

-- TODO: unknown format: application?
-- Cons -> {
--   (Cons!2885 (k!2883 (:var 0)) (k!2884 (:var 1)))
-- }

-- NOTE: Use `try` when alternatives share one prefix, otherwise the stream will be consumed

arrow :: Parser String
arrow = string "->"

parseModel :: String -> IO ()
parseModel input = case parse models "parse error" input of
                     Left err -> print err
                     Right val -> print val

type Model = (MIdent, MValue)

data MIdent = MIntSymbol Integer
            | MStringSymbol String
            | MElemSymbol Integer -- elem!i
              deriving Show

data MValue = MInt Integer
            | MBool Bool
            | MSort String Integer -- ident!val!i
            | MFun [MRule] -- [in] -> out. 1. [([],val)]: zero-arg fun. 2. [..., ([],val)]:
              deriving Show

type MRule = ([MValue], MValue)

models :: Parser [Model]
models = sepEndBy model newline

-- model ::= symbol -> value
model :: Parser Model
model = do
  n <- symbol
  space
  arrow
  space
  v <- value
  return (n, v)

-- symbol ::= intSymbol | stringSymbol
symbol :: Parser MIdent
symbol = try intSymbol <|> try elemSymbol <|> stringSymbol

-- intSymbol ::= k!int
intSymbol :: Parser MIdent
intSymbol = do
  string "k!"
  i <- digits
  return $ MIntSymbol i

digits :: Parser Integer
digits = liftM read $ many1 digit

stringSymbol :: Parser MIdent
stringSymbol = liftM  MStringSymbol ident

elemSymbol :: Parser MIdent
elemSymbol = do
  string "elem!"
  i <- digits
  return $ MElemSymbol i

-- stringSymbol ::= ()
-- value ::= int
--         | bool
--         | sort
--         | fun

value :: Parser MValue
value = try sort
        <|> fun
        <|> int
        <|> bool

-- int ::= digit+ | (- digit+)
int :: Parser MValue
int = liftM MInt digits <|> do string "(- "
                               n <- digits
                               char ')'
                               return $ MInt (-n)
-- bool ::= true | false
bool :: Parser MValue
bool =
    do b <- string "true" <|> string "false"
       return $ case b of
                  "true" -> MBool True
                  _ -> MBool False

-- sort ::= id!val!int
sort :: Parser MValue
sort = do
  n <- ident
  string "!val!"
  i <- digits
  return $ MSort n i


ident :: Parser String
ident = many (alphaNum <|> char '_')

-- fun ::= { rules }
fun :: Parser MValue
fun = do
  char '{'
  newline
  rs <- rules
  return $ MFun rs

rules :: Parser [MRule]
rules = spaces >>
        (try rule0 <|> do
           vs <- manyTill (value >>= \v -> space >> return v) (try arrow)
           space
           res <- value
           rs <- rules
           return $ (vs, res):rs)


-- rule0 ::= [else ->] value\n}
rule0 :: Parser [MRule]
rule0 = do
  optionMaybe (string "else -> ")
  res <- value
  newline
  char '}'
  return [([], res)]

-- The constant elem!0 is an auxiliary constant created by Z3 during the solving process [http://stackoverflow.com/questions/12899521/understanding-the-z3-model]

{- Examples -}
-- xs0 = Nil ==> False
test1 = "k!0 -> adtSort!val!0\n\
        \Nil -> adtSort!val!0\n\
        \elem!0 -> adtSort!val!2\n\
        \elem!1 -> adtSort!val!1\n\
        \Cons -> {\n\
        \  adtSort!val!3\n\
        \}"

-- xs0 = Cons x1 x2 ==> False
test2 = "k!0 -> adtSort!val!0\n\
        \k!1 -> adtSort!val!1\n\
        \k!2 -> adtSort!val!2\n\
        \Nil -> adtSort!val!3\n\
        \Cons_1 -> {\n\
        \  adtSort!val!1\n\
        \}\n\
        \Cons -> {\n\
        \  adtSort!val!0\n\
        \}\n\
        \Cons_2 -> {\n\
        \  adtSort!val!2\n\
        \}"

-- Look ahead tests
ident_test1 = "k!1 -> 1"
ident_test2 = "elem!0 -> 1"
ident_test3 = "kx -> 1"
ident_test4 = "elx -> 1"

value_test1 = "x -> 1"
value_test2 = "x -> (- 1)"
value_test3 = "x -> true"
value_test4 = "x -> 1!val!1"
value_test5 = "x -> false!val!1"
value_test6 = "f -> {\n\
              \  4\n}"
value_test7 = "f -> {\n\
              \  1 2 -> 3\n\
              \  else -> 4\n}"
