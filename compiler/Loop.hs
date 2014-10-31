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
import Text.PrettyPrint.Leijen
import Translations
import JavaUtils
import Parser
import ParseCMD
import FileIO
import qualified Environment as Env
import qualified History as Hist 

loop :: Connection -> ValueContext -> Env.Env -> Hist.Hist -> Int -> Bool -> Bool -> Bool -> Int -> InputT IO ()
loop (inP, outP) val_cxt env hist index flagH flagT flagS num = do
	let max = length hist	
	case flagH of
	  True  -> case (index == max)  of
	    True  -> loop (inP, outP) val_cxt env hist 0 False flagT flagS num
	    False -> do
	      let cmd = (reverse hist) !! index
	      outputStrLn ""
	      outputStrLn ("Replaying: " ++ cmd)
	      runCMD (inP, outP) val_cxt env hist (index+1) True flagT flagS num cmd
	  False -> do
	    msg <- getInputLine "f2ji> "
	    case msg of
	      Nothing    -> return ()
	      Just ""    -> loop (inP, outP) val_cxt env hist 0 False flagT flagS num
	      Just input -> do
	        case input of 
		  ":replay" -> runCMD (inP, outP) val_cxt env hist 0 
		  		      False flagT flagS num input
	          other     -> do 
		    let histNew = Hist.insert hist input
	            runCMD (inP, outP) val_cxt env histNew 0 
		            False flagT flagS num input

runCMD :: Connection -> ValueContext -> Env.Env -> Hist.Hist -> Int -> Bool -> Bool -> Bool -> Int -> String -> InputT IO ()
runCMD (inP, outP) val_ctx env hist index flagH flagT flagS num msg = do
	case (stripPrefix ":" msg) of
	  Just input -> 
		case parseMsg msg of
		  Left err   ->  outputStrLn "Parse error"
		  Right line -> processCMD (inP, outP) val_ctx env hist index
		  			   flagH flagT flagS num line
	  Nothing   -> do 
	  	result <- liftIO (checkType val_ctx msg)
		case result of
		  Left typeErr 	      -> do
			outputStrLn (show typeErr)
			loop (inP, outP) val_ctx env hist index flagH flagT flagS num
		  Right (tchecked, t) -> do 
			let filename = "main_" ++ (show num) ++ ".sf"
			let bind = Env.reverseEnv env
			let msgEnv = (Env.createBindEnv bind) ++ msg
	  		liftIO (writeFile filename msgEnv)
	  		liftIO (wrapFlag (inP, outP) flagT flagS filename)
			exist <- liftIO (doesFileExist filename)
			if exist 
		  	  then do liftIO (removeFile filename)
			    	  loop (inP, outP) val_ctx env hist index 
				       flagH flagT flagS (num+1)
		    	  else loop (inP, outP) val_ctx env hist index 
			            flagH flagT flagS (num+1)

processCMD :: Connection -> ValueContext -> Env.Env -> Hist.Hist -> Int -> Bool -> Bool -> Bool -> Int -> [String] -> InputT IO ()
processCMD (inP, outP) val_ctx env hist index flagH flagT flagS num (x : xs) = do
	case x of 
	  ":help" -> do 
		liftIO printHelp
		loop (inP, outP) val_ctx env hist index flagH flagT flagS num
	  ":send" -> do
		case getCMD xs of
		  Just filename -> liftIO (wrapFlag (inP, outP) flagT flagS filename) 
	      	  Nothing       ->  outputStrLn "Invalid input"
		loop (inP, outP) val_ctx env hist index flagH flagT flagS num
	  ":time" -> case getCMD xs of 
		Just "on"  -> loop (inP, outP) val_ctx env hist index
				   flagH True flagS num
		Just "off" -> loop (inP, outP) val_ctx env hist index
				   flagH False flagS num
		_          -> do 
		  outputStrLn "Invalid input"
		  loop (inP, outP) val_ctx env hist index flagH flagT flagS num
	  ":quit" -> return ()
	  ":showfile" -> case getCMD xs of
	  	Just "on"  -> loop (inP, outP) val_ctx env hist index 
				   flagH flagT True num
		Just "off" -> loop (inP, outP) val_ctx env hist index
				   flagH flagT False num
		_	   -> do 
			outputStrLn "Invalid input"
			loop (inP, outP) val_ctx env hist index flagH flagT flagS num
	  ":let"  -> do 
	  	if (length xs) < 3 
		  then do
		    outputStrLn "Parse error: no space around \"=\"/Too few input"
                    loop (inP, outP) val_ctx env hist index flagH flagT flagS num
		  else do  
		    let (var, exp) = Env.createPair xs
		    --outputStrLn exp
		    result <- liftIO (checkType val_ctx exp)
		    case result of
		      Left  typeErr	  -> do
		        --outputStrLn "typeCheck error!"
		        outputStrLn (show typeErr)
			loop (inP, outP) val_ctx env hist index flagH flagT flagS num
		      Right (tchecked, t) -> do
		        let val_ctx_new = Map.insert var t val_ctx 
			--outputStrLn (show val_ctx_new)
	  	      	let envNew = Env.insert (var, exp) env
		        loop (inP, outP) val_ctx_new envNew hist index 
			     flagH flagT flagS num	
	  ":type" -> do
	  	case getCMD xs of
	  	  Just var -> case Map.lookup var val_ctx of
		    Just t  -> outputStrLn (show (pretty t))
		    Nothing -> outputStrLn "variable not found" 
		  Nothing  ->  outputStrLn "Too few input"
		loop (inP, outP) val_ctx env hist index flagH flagT flagS num
	  ":show" -> case getCMD xs of
	  	Just "env" -> do 
		  outputStrLn ("[" ++ Env.showPrettyEnv env ++ "]")
		  loop (inP, outP) val_ctx env hist index flagH flagT flagS num
		Just input -> outputStrLn "Invalid input"
		Nothing    -> outputStrLn "Too few input"
	  ":clear" -> loop (inP, outP) Map.empty Env.empty hist index
	  	           flagH flagT flagS num
	  ":replay" -> do
	  	let envNew = Env.empty
		--outputStrLn (show hist)
		loop (inP, outP) val_ctx envNew hist 0 True flagT flagS num
	  input	  -> do outputStrLn $ "Command not recognized: " ++ input
	  	        loop (inP, outP) val_ctx env hist index
			     flagH flagT flagS num

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
	putStrLn "  :type var           Show the type of var"
	putStrLn "  :replay             Replay all previous user commands"
	putStrLn "  :showfile on/off    Show/Hide source file and .java file contents"
	putStrLn "  :time on/off        Show/Hide execution time"
	putStrLn "  :show env           Show current environment"
	putStrLn "  :quit               Quit f2ji"
	putStrLn "-----------------------------------------"
	putStrLn ""


