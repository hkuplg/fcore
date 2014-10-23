module Environment where

import System.IO
import Data.List.Split

type Env = [(String, String)]
    
empty :: Env
empty = []

insert :: (String, String) -> Env -> Env
insert (var, exp) env = (var, exp) : env

createExp :: [String] -> String
createExp [] = ""
createExp (x:xs) = x ++ createExp xs

-- y is "="
-- Sample input: ["x", "=", "y" , "+", "2"] 
-- Sample output: ("x", "y+2")
createPair :: [String] -> (String, String)
createPair xs = (var, exp) where
	var = createExp ((splitOn ["="] xs) !! 0)
	exp = createExp ((splitOn ["="] xs) !! 1)

reverseEnv :: Env -> Env
reverseEnv env = reverse env

createBindEnv :: Env -> String
createBindEnv [] = ""
createBindEnv ((var,exp) : xs) = "let " ++ var ++ " = " ++ exp 
			   		++ " in " ++ createBindEnv xs   
searchEnv :: String -> Env -> Bool
searchEnv var env = case lookup var env of
			Nothing  -> False
			Just exp -> True


