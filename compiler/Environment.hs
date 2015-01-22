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
insert (var, exp) env = (var, (exp, Parser.reader exp)) : env

createExp :: [String] -> String
createExp [] = ""
createExp (x:xs) = x ++ " " ++ createExp xs

createPair :: [String] -> (String, String)
createPair xs = (var, exp) where
        var = case firstElem 0 "=" xs of
                -1  -> "" 
                num -> let func = unwords . fst . (splitAt num) in func xs
        exp = case firstElem 0 "=" xs of
	        -1  -> ""
		num -> let func = unwords . tail . snd . (splitAt num) in func xs

firstElem :: Int -> String -> [String] -> Int
firstElem _ _ [] = -1
firstElem num elem (x:xs) = if x == elem then num else firstElem (num+1) elem xs 

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
