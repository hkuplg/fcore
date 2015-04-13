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

import           Control.Applicative           ((<$>))
import           Data.Char                     (toLower)
import qualified Data.IntMap                   as IntMap
import           Text.ParserCombinators.Parsec

-- TODO: unknown format: application?
-- Cons -> {
--   (Cons!2885 (k!2883 (:var 0)) (k!2884 (:var 1)))
-- }
-- TODO: unknown format: let bindings
-- k!1 -> (let ((a!1 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
--              (...))
--          (Cons 0 a!1))

-- NOTE: Use `try` when alternatives share one prefix, otherwise the stream will be consumed

parseModel :: String -> [Bind]
parseModel input = case parse binds "parse error" input of
                     Left err -> error $ show err
                     Right bs -> bs

type Bind = (MIdent, MExpr)

data MIdent = MIntSymbol Int
            | MStringSymbol String
            | MElemSymbol Int -- elem!i
            | MLetSymbol Int -- a!i
-- The constant elem!i is an auxiliary constant created by Z3 during the solving process [http://stackoverflow.com/questions/12899521/understanding-the-z3-model]

isMIntSymbol :: MIdent -> Bool
isMIntSymbol (MIntSymbol _) = True
isMIntSymbol _ = False

data MExpr = MApp MOp [MExpr]
           | MLet [Bind] MExpr -- a!i
           | MValue MValue

counterExample :: Int -> String -> String
counterExample total = ("Counter example:\n" ++ ) . unlines . map (\(i,e) -> "x" ++ show i ++ " = " ++ show e) . relevantBinds total . parseModel

relevantBinds :: Int -> [Bind] -> [(Int, MExpr)]
relevantBinds total bs =
    [(i, substLet e) | (b, e) <- bs, isMIntSymbol b, let MIntSymbol i = b, i < total]

substLet :: MExpr -> MExpr
substLet = go IntMap.empty
    where go env (MLet bs e) = go (foldr (\(MLetSymbol i, e') acc -> IntMap.insert i (go env e') acc) env bs) e
          go env e@(MValue v) =
              case v of
                MIdent (MLetSymbol i) -> env IntMap.! i
                _ -> e
          go env (MApp op es) = MApp op (map (go env) es)

data MOp = MOp String
         | Minus
         | Mult

data MValue = MInt Int
            | MBool Bool
            | MSort String Int -- ident!val!i
            | MIdent MIdent
            | MFun [MRule] -- [in] -> out 1. [([],val)]: zero-arg fun. 2. [..., ([],val)]:
type MRule = ([MExpr], MExpr)

showBinds :: [Bind] -> String
showBinds = init . unlines . map showBind

showBind :: Bind -> String
showBind (x,v) = show x ++ " -> " ++ show v

instance Show MIdent where
    show (MIntSymbol i) = "k!" ++ show i
    show (MStringSymbol s) = s
    show (MElemSymbol i) = "elem!" ++ show i
    show (MLetSymbol i) = "a!" ++ show i

instance Show MExpr where
    show (MApp op vs) = "(" ++ unwords (show op : map show vs) ++ ")"
    show (MLet [(i,e)] expr) = "(let (" ++ ("(" ++ show i ++ " " ++ show e ++ ")") ++ ")\n" ++ "  " ++ show expr ++ ")"
    show (MValue v) = show v

instance Show MValue where
    show (MInt i) = show i
    show (MBool b) = let c:cs = show b in toLower c : cs
    show (MSort s i) = s ++ "!val!" ++ show i
    show (MFun [([], v)]) = "{" ++ "\n  " ++ show v ++ "\n" ++ "}"
    show (MFun rs) = unlines ("{" : map showRule rs) ++ "}"
    show (MIdent sym) = show sym

instance Show MOp where
    show (MOp s) = s
    show Minus = "-"
    show Mult = "*"

showRule :: MRule -> String
showRule ([], e) = "  _ -> " ++ show e
showRule (es, e) = "  " ++ unwords (map show es) ++ " -> " ++ show e

arrow :: Parser String
arrow = string "->"

-- parens p = between (char ')') (char '(') p

binds :: Parser [Bind]
binds = sepEndBy bind newline

-- model ::= symbol " -> " value
bind :: Parser Bind
bind = do
  n <- symbol
  space
  arrow
  space
  e <- expr
  return (n, e)

-- symbol ::= intSymbol | stringSymbol | letSymbol | stringSymbol
symbol :: Parser MIdent
symbol = intSymbol <|> elemSymbol <|> letSymbol <|> stringSymbol

-- intSymbol ::= k!int
intSymbol :: Parser MIdent
intSymbol = do
  try $ string "k!"
  i <- digits
  return $ MIntSymbol i

-- opSymbol :: Parser MIdent
-- opSymbol = oneOf "-*/+" >>= \op -> return $ MOp op

digits :: Parser Int
digits = read <$> many1 digit

stringSymbol :: Parser MIdent
stringSymbol = MStringSymbol <$> ident

elemSymbol :: Parser MIdent
elemSymbol = do
  try $ string "elem!"
  i <- digits
  return $ MElemSymbol i

-- expr ::= app
--        | let (letbinds) expr
--        | value
expr :: Parser MExpr
expr = try letExpr <|> app <|> MValue <$> value

letExpr :: Parser MExpr
letExpr = do
  char '('
  string "let "
  char '('
  -- binds <- many1 (letBind >>= \b -> many (newline <|> space) >> b)
  bind <- letBind
  char ')'
  newline
  spaces
  e <- expr
  char ')'
  return $ MLet [bind] e

letBind = do
  char '('
  sym <- letSymbol
  space
  e <- expr
  char ')'
  return (sym, e)

letSymbol :: Parser MIdent
letSymbol = do
  string "a!"
  i <- digits
  return $ MLetSymbol i

-- app ::= (op values)
app :: Parser MExpr
app = do char '('
         o <- op
         es <- many1 (space >> expr)
         char ')'
         return $ MApp o es
-- op ::= UpperId
--      | -
op :: Parser MOp
op = (char '-' >> return Minus) <|> (char '*' >> return Mult) <|> (MOp <$> ident)

-- value ::= int
--         | bool
--         | sort
--         | let
--         | app
--         | fun
--         | symbol
value :: Parser MValue
value = fun
        <|> int
        <|> try bool
        <|> try sort
        <|> MIdent <$> symbol

-- int ::= digit+ | (- digit+)
int :: Parser MValue
int = MInt <$> digits

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
  rs <- rules
  char '}'
  return $ MFun rs

rules :: Parser [MRule]
rules = many1 (try rule0 <|> rule)

-- rules ::= spaces [value space]+ -> value
rule :: Parser MRule
rule = do
  spaces
  es <- many1 (expr >>= \e -> space >> return e)
  arrow
  space
  res <- expr
  newline
  return (es, res)

-- rule0 ::= [else ->] value\n}
rule0 :: Parser MRule
rule0 = do
  spaces
  optionMaybe (string "else -> ")
  res <- expr
  newline
  return ([], res)

showParsed = showBinds . parseModel
putParsed = putStrLn . showParsed
