{- |
Module      :  Environment
Description :  REPL environment
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Boya Peng <u3502350@connect.hku.hk>
Stability   :  stable
Portability :  portable
-}

module Environment where

import Data.List.Split

import Parser
import Src
import Text.PrettyPrint.ANSI.Leijen

type Env = [(String, Exp)]
type Exp = (String, Src.ReaderExpr)

empty :: Env
empty = []

insert :: (String, String) -> Env -> Env
insert (var, exp) env = 
  case Parser.reader exp of
    Left  expr           -> (var, (exp, expr)) : env
    Right (PError error) -> env

createExp :: [String] -> String
createExp [] = ""
createExp (x:xs) = x ++ " " ++ createExp xs

-- y is "="
-- Sample input: ["x", "=", "y" , "+", "2"]
-- Sample output: splitOn ["="] xs ~> [["x"], ["y", "+", "2"]]
-- Sample output: ("x", "y+2")
createPair :: [String] -> (String, String)
createPair xs = (var, exp) where
        var = ((splitOn ["="] xs) !! 0) !! 0
        exp = createExp ((splitOn ["="] xs) !! 1)

reverseEnv :: Env -> Env
reverseEnv env = reverse env

createBindEnv :: Env -> String
createBindEnv [] = ""
createBindEnv ((var,(str,expr)) : xs) = "let " ++ var ++ " = " ++ str
                                        ++ " in " ++ createBindEnv xs
searchEnv :: String -> Env -> Bool
searchEnv var env = case lookup var env of
                        Nothing  -> False
                        Just exp -> True

showPrettyEnv :: Env -> String
showPrettyEnv [] = ""
showPrettyEnv ((var, (str, expr)) : xs) = "(" ++ show var ++ ", "
                                              ++ show (pretty expr)
                                              ++ "); " ++ showPrettyEnv xs
