module Loop where

import System.Console.Haskeline		
import System.IO
import System.Process hiding (runCommand)
import System.TimeIt
import System.Directory			(removeFile, doesFileExist)

import Control.Monad.Error		(liftIO)

import Data.Char
import Data.List.Split
import Data.List

import TypeCheck
import Translations
import JavaUtils
import Parser
import ParseCMD
import FileIO
import Environment as Env

loop :: Connection -> Env -> Bool -> Bool -> Int -> InputT IO ()
loop (inP, outP) env flagT flagS num = do
	msg <- getInputLine "% "
	case msg of
	  Nothing    -> return ()
	  Just ""    -> loop (inP, outP) env flagT flagS num
	  Just input -> runCommand (inP, outP) env flagT flagS num input
	  	 	
runCommand :: Connection -> Env -> Bool -> Bool -> Int -> String -> InputT IO ()
runCommand (inP, outP) env flagT flagS num msg = do
	case (stripPrefix ":" msg) of
	  Just input -> 
		case parseMsg msg of
		  Left err   ->  outputStrLn "Parse error"
		  Right line -> processCMD (inP, outP) env flagT flagS num line
	  Nothing -> do 
		let fileName = "main_" ++ (show num) ++ ".sf"
		let bind = reverseEnv env
		let msgEnv = (createBindEnv bind) ++ msg
	  	liftIO (writeFile fileName msgEnv)
	  	case flagT of 
		  True 	-> liftIO (timeIt (wrap (inP, outP) flagS fileName))
		  False -> liftIO (wrap (inP, outP) flagS fileName)
		exist <- liftIO (doesFileExist fileName)
		if exist 
		  then do liftIO (removeFile fileName)
			  loop (inP, outP) env flagT flagS (num+1)
		  else loop (inP, outP) env flagT flagS (num+1)

processCMD :: Connection -> Env -> Bool -> Bool -> Int -> [String] -> InputT IO ()
processCMD (inP, outP) env flagT flagS num (x : xs) = do
	case x of 
	  ":help" -> do 
		liftIO printHelp
		loop (inP, outP) env flagT flagS num
	  ":send" -> do
		case getCMD xs of
		  Just filename -> do 
	      		case flagT of
			  True	-> liftIO (timeIt (wrap (inP, outP) flagS filename))
	  		  False -> liftIO (wrap (inP, outP) flagS filename)
		  Nothing       ->  outputStrLn "Invalid input"
		loop (inP, outP) env flagT flagS num
	  ":time" -> case getCMD xs of 
		Just "on"  -> loop (inP, outP) env True flagS num
		Just "off" -> loop (inP, outP) env False flagS num
		_          -> do outputStrLn "Invalid input"
			         loop (inP, outP) env flagT flagS num
	  ":quit" -> return ()
	  ":showfile" -> case getCMD xs of
	  	Just "on"  -> loop (inP, outP) env flagT True num
		Just "off" -> loop (inP, outP) env flagT False num
	  ":let"  -> do 
	  	if (length xs) < 3 
		  then do
		    outputStrLn "Parse error: no space around \"=\"/Too few input"
                    loop (inP, outP) env flagT flagS num	
		  else do  
		    let (var, exp) = createPair xs
	  	    let envNew = Env.insert (var, exp) env
		    loop (inP, outP) envNew flagT flagS num	
	  ":show" -> case getCMD xs of
	  	Just "env" -> do outputStrLn (show env)
				 loop (inP, outP) env flagT flagS num
	  input	  -> do outputStrLn $ "Command not recognized: " ++ input
	  	        loop (inP, outP) env flagT flagS num

getCMD :: [String] -> Maybe String
getCMD xs = case xs of
		[x] -> Just x
		xs  -> Nothing

checkType :: String -> IO Bool
checkType s =
  do let parsed = reader s
     r <- typeCheck parsed
     case r of
       Left typeError       -> return False  
       Right (tchecked, _t) -> return True 

printHelp :: IO ()
printHelp = do
	putStrLn ""
	putStrLn "Welcome to f2ji!"
	putStrLn "-----------------------------------------"
	putStrLn "[COMMANDS] [SOURCE FILE/FLAG]"
	putStrLn "Options:"
	putStrLn "  :help               Print help manual"
	putStrLn "  :send sourceFile    Load sourceFile"
	putStrLn "  :let var = expr     Bind expr to var"
	putStrLn "  :showfile on/off    Show/Hide source file and .java file contents"
	putStrLn "  :show env           Show current environment"
	putStrLn "  :time on            Show execution time"
	putStrLn "  :time off           Hide execution time"
	putStrLn "  :quit               Quit f2ji"
	putStrLn "-----------------------------------------"
	putStrLn ""


