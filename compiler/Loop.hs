module Loop where

import System.Console.Haskeline		
import System.IO
import System.Process hiding (runCommand)
import System.TimeIt
import System.Directory			(removeFile, doesFileExist)

import Control.Monad.Error		(liftIO)

import Data.Char
import Data.List.Split
import Data.List			(stripPrefix)
import qualified Data.Map as Map

import TypeCheck
import Src hiding (wrap)
import Translations
import JavaUtils
import Parser
import ParseCMD
import FileIO
import Environment as Env

loop :: Connection -> ValueContext -> Env -> Bool -> Bool -> Int -> InputT IO ()
loop (inP, outP) val_cxt env flagT flagS num = do
	msg <- getInputLine "% "
	case msg of
	  Nothing    -> return ()
	  Just ""    -> loop (inP, outP) val_cxt env flagT flagS num
	  Just input -> runCommand (inP, outP) val_cxt env flagT flagS num input
	  	 	
runCommand :: Connection ->ValueContext -> Env -> Bool -> Bool -> Int -> String -> InputT IO ()
runCommand (inP, outP) val_ctx env flagT flagS num msg = do
	case (stripPrefix ":" msg) of
	  Just input -> 
		case parseMsg msg of
		  Left err   ->  outputStrLn "Parse error"
		  Right line -> processCMD (inP, outP) val_ctx env 
		  			   flagT flagS num line
	  Nothing   -> do 
		let filename = "main_" ++ (show num) ++ ".sf"
		let bind = reverseEnv env
		let msgEnv = (createBindEnv bind) ++ msg
	  	liftIO (writeFile filename msgEnv)
	  	liftIO (wrapFlag (inP, outP) flagT flagS filename)
		exist <- liftIO (doesFileExist filename)
		if exist 
		  then do liftIO (removeFile filename)
			  loop (inP, outP) val_ctx env flagT flagS (num+1)
		  else loop (inP, outP) val_ctx env flagT flagS (num+1)

processCMD :: Connection -> ValueContext -> Env -> Bool -> Bool -> Int -> [String] -> InputT IO ()
processCMD (inP, outP) val_ctx env flagT flagS num (x : xs) = do
	case x of 
	  ":help" -> do 
		liftIO printHelp
		loop (inP, outP) val_ctx env flagT flagS num
	  ":send" -> do
		case getCMD xs of
		  Just filename -> liftIO (wrapFlag (inP, outP) flagT flagS filename) 
	      	  Nothing       ->  outputStrLn "Invalid input"
		loop (inP, outP) val_ctx env flagT flagS num
	  ":time" -> case getCMD xs of 
		Just "on"  -> loop (inP, outP) val_ctx env True flagS num
		Just "off" -> loop (inP, outP) val_ctx env False flagS num
		_          -> do outputStrLn "Invalid input"
			         loop (inP, outP) val_ctx env flagT flagS num
	  ":quit" -> return ()
	  ":showfile" -> case getCMD xs of
	  	Just "on"  -> loop (inP, outP) val_ctx env flagT True num
		Just "off" -> loop (inP, outP) val_ctx env flagT False num
	  ":let"  -> do 
	  	if (length xs) < 3 
		  then do
		    outputStrLn "Parse error: no space around \"=\"/Too few input"
                    loop (inP, outP) val_ctx env flagT flagS num	
		  else do  
		    let (var, exp) = createPair xs
		    --outputStrLn exp
		    result <- liftIO (checkType val_ctx exp)
		    case result of
		      Left  typeErr	  -> do
		        outputStrLn (show typeErr)
			loop (inP, outP) val_ctx env flagT flagS num
		      Right (tchecked, t) -> do
		        let val_ctx_new = Map.insert var t val_ctx 
			--outputStrLn (show val_ctx_new)
	  	      	let envNew = Env.insert (var, exp) env
		        loop (inP, outP) val_ctx_new envNew flagT flagS num	
	  ":show" -> case getCMD xs of
	  	Just "env" -> do outputStrLn (show env)
				 loop (inP, outP) val_ctx env flagT flagS num
		Just input -> outputStrLn "Invalid input"
		Nothing    -> outputStrLn "Too few input"
	  ":clear" -> loop (inP, outP) Map.empty Env.empty flagT flagS num
	  input	  -> do outputStrLn $ "Command not recognized: " ++ input
	  	        loop (inP, outP) val_ctx env flagT flagS num

getCMD :: [String] -> Maybe String
getCMD xs = case xs of
		[x] -> Just x
		xs  -> Nothing

wrapFlag :: Connection -> Bool -> Bool -> String -> IO ()
wrapFlag (inP, outP) flagT flagS filename = case flagT of
	True  -> timeIt (wrap (inP, outP) flagS filename)
	False -> wrap (inP, outP) flagS filename

checkType :: ValueContext -> String -> IO (Either TypeError (Expr TcId, Type))
checkType val_ctx s =
  do let parsed = reader s
     typeCheckWithEnv val_ctx parsed
     
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
	putStrLn "  :time on/off        Show/Hide execution time"
	putStrLn "  :quit               Quit f2ji"
	putStrLn "-----------------------------------------"
	putStrLn ""


