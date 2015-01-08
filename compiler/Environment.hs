module Environment where

import Data.List.Split

import Parser
import Src
import Text.PrettyPrint.ANSI.Leijen

type Env = [(String, Exp)]
type Exp = (String, Src.Expr Src.Name)

empty :: Env
empty = []

insert :: (String, String) -> Env -> Env
insert (var, exp) env = (var, (exp, Parser.reader exp)) : env

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

