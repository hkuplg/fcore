{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Loop where

import System.Console.Haskeline
import System.IO
import System.Process hiding (runCommand)
import System.TimeIt
import System.CPUTime
import System.Directory			(removeFile, doesFileExist)

import Control.Monad.Error		(liftIO)
import qualified Control.Exception as E

import Data.Char
import Data.List.Split
import Data.List			(stripPrefix, group, sort)
import qualified Data.Map as Map

import TypeCheck
import Src hiding (wrap)
import Text.PrettyPrint.Leijen
import Translations
import JavaUtils
import qualified OptiUtils		(sf2core)
import qualified Core			(prettyExpr)
import Parser
import ParseCMD
import FileIO
import qualified Environment as Env
import qualified History as Hist
import FileIO 				(TransMethod (Apply, Naive, Stack, Unbox, StackAU1, StackAU2, BenchN, BenchS, BenchNA, BenchSA, BenchSAI1, BenchSAI2))

#ifdef Z3
-- #if MIN_VERSION_z3(0,3,2)
import Z3Backend
#endif

loop :: Connection -> CompileOpt -> ValueContext -> Env.Env -> Hist.Hist -> Int -> Bool -> Bool -> Bool -> Int -> InputT IO ()
loop handle opt val_cxt env hist index flagH flagT flagS num = do
	let max = length hist
	case flagH of
	  True  -> case (index == max)  of
	    True  -> loop handle opt val_cxt env hist 0 False flagT flagS num
	    False -> do
	      let cmd = (reverse hist) !! index
	      outputStrLn ""
	      outputStrLn ("Replaying: " ++ cmd)
	      runCMD handle opt val_cxt env hist (index+1) True flagT flagS num cmd
	  False -> do
	    msg <- getInputLine "f2ji> "
	    case msg of
	      Nothing    -> return ()
	      Just ""    -> loop handle opt val_cxt env hist 0 False flagT flagS num
	      Just input -> do
	        case input of
		  ":replay" -> runCMD handle opt val_cxt env hist 0
		  	              False flagT flagS num input
	          other     -> do
		    let histNew = Hist.insert hist input
	            runCMD handle opt val_cxt env histNew 0
		           False flagT flagS num input

runCMD :: Connection -> CompileOpt -> ValueContext -> Env.Env -> Hist.Hist -> Int -> Bool -> Bool -> Bool -> Int -> String -> InputT IO ()
runCMD handle opt val_ctx env hist index flagH flagT flagS num msg = do
	case (stripPrefix ":" msg) of
	  Just input ->
		case parseMsg msg of
		  Left err   ->  outputStrLn "Parse error"
		  Right line -> processCMD handle opt val_ctx env hist index
		  			   flagH flagT flagS num line
	  Nothing   -> do
	  	result <- liftIO (checkType val_ctx msg)
		case result of
		  Left typeErr 	      -> do
			outputStrLn (show typeErr)
			loop handle opt val_ctx env hist index flagH flagT flagS num
		  Right (tchecked, t) -> do
			let filename = "main_" ++ (show num) ++ ".sf"
			let bind = Env.reverseEnv env
			let msgEnv = (Env.createBindEnv bind) ++ msg
	  		liftIO (writeFile filename msgEnv)
	  		liftIO (wrapFlag handle opt flagT flagS filename)
			exist <- liftIO (doesFileExist filename)
			if exist
		  	  then do liftIO (removeFile filename)
			    	  loop handle opt val_ctx env hist index
				       flagH flagT flagS (num+1)
		    	  else loop handle opt val_ctx env hist index
				    flagH flagT flagS (num+1)

processCMD :: Connection -> CompileOpt -> ValueContext -> Env.Env -> Hist.Hist -> Int -> Bool -> Bool -> Bool -> Int -> [String] -> InputT IO ()
processCMD handle opt val_ctx env hist index flagH flagT flagS num (x : xs) = do
	case x of
	  ":help" -> do
		liftIO printHelp
		loop handle opt val_ctx env hist index
		     flagH flagT flagS num
	  ":run" -> do
		case getCMD xs of
		  Just filename -> liftIO (wrapFlag handle opt flagT flagS filename)
	      	  Nothing       ->  outputStrLn "Invalid input"
		loop handle opt val_ctx env hist index flagH flagT flagS num
#ifdef Z3
-- #if MIN_VERSION_z3(0,3,2)
          ":se" -> do
	  	case getCMD xs of
		  Just filename -> do
		    expr <- liftIO (OptiUtils.sf2core filename)
		    liftIO $ solve expr
		  Nothing 	-> outputStrLn "Invalid input"
		loop handle opt val_ctx env hist index flagH flagT flagS num
#endif
	  ":expr" -> do
	  	case getCMD xs of
		  Just filename -> do
		    expr <- liftIO (OptiUtils.sf2core filename)
		    outputStrLn (show (Core.prettyExpr expr))
		  Nothing 	-> outputStrLn "Invalid input"
		loop handle opt val_ctx env hist index flagH flagT flagS num
	  ":set" -> case getCMD xs of
	 	Just "method" -> do
		  let (y:ys)= xs
		  --case getCMD ys of
		  let ms = parseMethod ys
		  result <- liftIO (E.try (getOpt ms))
		  case result of
		    Left (_ :: E.SomeException) -> do
		      outputStrLn "Invalid method"
		      loop handle opt val_ctx env hist index flagH flagT flagS num
		    Right optNew 		-> do
		      loop handle optNew val_ctx env hist index flagH flagT flagS num
		_	      -> do
		  outputStrLn "Invalid input"
		  loop handle opt val_ctx env hist index flagH flagT flagS num
	  ":quit" -> return ()
	  ":let"  -> do
	  	if (length xs) < 3
		  then do
		    outputStrLn "Parse error: no space around \"=\"/Too few input"
                    loop handle opt val_ctx env hist index flagH flagT flagS num
		  else if (xs !! 1) /= "="
		    then do
		      outputStrLn "Parse error: no space around \"=\""
                      loop handle opt val_ctx env hist index flagH flagT flagS num
		    else do
		      let (var, exp) = Env.createPair xs
		      --outputStrLn exp
		      result <- liftIO (checkType val_ctx exp)
		      case result of
		        Left  typeErr	  -> do
		          --outputStrLn "typeCheck error!"
		          outputStrLn (show typeErr)
			  loop handle opt val_ctx env hist index flagH flagT flagS num
		        Right (tchecked, t) -> do
		          let val_ctx_new = Map.insert var t val_ctx
			  --outputStrLn (show val_ctx_new)
	  	      	  let envNew = Env.insert (var, exp) env
		          loop handle opt val_ctx_new envNew hist index
			       flagH flagT flagS num
	  ":type" -> do
	  	case getCMD xs of
	  	  Just var -> case Map.lookup var val_ctx of
		    Just t  -> outputStrLn (show (pretty t))
		    Nothing -> outputStrLn "variable not found"
		  Nothing  ->  outputStrLn "Too few input"
		loop handle opt val_ctx env hist index flagH flagT flagS num
	  ":show" -> case getCMD xs of
	        Just "time"   -> do
		  let (y:ys) = xs
		  case getCMD ys of
		    Just "on"  -> loop handle opt val_ctx env hist index
				       flagH True flagS num
		    Just "off" -> loop handle opt val_ctx env hist index
				       flagH False flagS num
		    _	     -> do
		      outputStrLn "Invalid option for :set time"
               	      loop handle opt val_ctx env hist index flagH flagT flagS num
	  	Just "file"   -> do
		  let (y:ys) = xs
		  case getCMD ys of
		    Just "on"  -> loop handle opt val_ctx env hist index
		                  flagH flagT True num
		    Just "off" -> loop handle opt val_ctx env hist index
				  flagH flagT False num
		    _          -> do
		      outputStrLn "Invalid option for :show file"
		      loop handle opt val_ctx env hist index flagH flagT flagS num
	  	Just "env"    -> do
		  outputStrLn ("[" ++ Env.showPrettyEnv env ++ "]")
		  loop handle opt val_ctx env hist index flagH flagT flagS num
		Just "method" -> do
		  let (num, compile, method) = opt
		  outputStrLn ("Currently using: " ++ show method)
		  outputStrLn "----------------------------"
		  outputStrLn "Avaible compilation options:"
		  outputStrLn "(Can also be the combination of the first four methods)"
		  outputStrLn "----------------------------"
		  outputStrLn "naive"
		  outputStrLn "apply"
		  outputStrLn "unbox"
		  outputStrLn "stack"
		  outputStrLn "benchs"
		  outputStrLn "benchna"
		  outputStrLn "benchsa"
		  outputStrLn "benchsai1"
		  outputStrLn "benchsai2"
		  outputStrLn "---------------------------"
		  outputStrLn "Default: naive"
		  loop handle opt val_ctx env hist index flagH flagT flagS num
		Just input -> do
		  exist <- liftIO (doesFileExist input)
		  case exist of
		    True  -> do
		   	content <- liftIO (readFile input)
		        outputStrLn content
		    False -> outputStrLn "Invalid input"
		  loop handle opt val_ctx env hist index flagH flagT flagS num
		Nothing    -> do
		  outputStrLn "Too few input"
		  loop handle opt val_ctx env hist index flagH flagT flagS num
	  ":clear" -> loop handle opt Map.empty Env.empty hist index
	  	           flagH flagT flagS num
	  ":replay" -> do
	  	let envNew = Env.empty
		--outputStrLn (show hist)
		loop handle opt val_ctx envNew hist 0 True flagT flagS num
	  input	  -> do outputStrLn $ "Command not recognized: " ++ input
	  	        loop handle opt val_ctx env hist index flagH flagT flagS num

getCMD :: [String] -> Maybe String
getCMD xs = case xs of
		[]      -> Nothing
		(x:xs)  -> Just x

parseMethod :: [String] -> [String]
parseMethod xs = (map head . Data.List.group . sort) xs;

-- if the return type is CompileOpt, then return to E.try (return (getOpt method))
-- will not evaluate argument inside return (lazy), thus can't catch error
getOpt :: [String] -> IO CompileOpt
getOpt ms = case ms of
	["naive"] 		-> return (0, compileN, [Naive])
	["apply"] 		-> return (0, compileAO, [Apply, Naive])
	["apply", "unbox"]	-> return (0, compileAoptUnbox, [Apply, Naive, Unbox])
	["apply", "stack"]  	-> return (0, compileS, [Apply, Naive, Stack])
	["apply", "stack", "unbox"] 
			        -> return (0, compileSAU, [Apply, Naive, Stack, Unbox])
	["stackau1"] 		-> return (1, compileSAU, [Naive, StackAU1])
	["stackau2"] 		-> return (2, compileSAU, [Naive, StackAU2])
	["stack"]		-> return (0, compileSN, [Naive, Stack])
	["stack", "unbox"] 	-> return (0, compileSU, [Naive, Stack, Unbox])
	["unbox"]		-> return (0, compileUnbox, [Naive, Unbox])
	["benchs"] 		-> return (0, (compileBS False), [BenchS])
	["benchna"] 		-> return (0, (compileBN True), [BenchNA])
	["benchSA"]	 	-> return (0, (compileBS True), [BenchSA])
	["benchSAI1"]		-> return (1, (compileBS True), [BenchSAI1])
	["benchSAI2"] 		-> return (2, (compileBS True), [BenchSAI2])
	_			-> error "invalid method" 

wrapFlag :: Connection -> CompileOpt -> Bool -> Bool -> String -> IO ()
wrapFlag handle opt flagT flagS filename = case flagT of
	True  -> --timeIt (wrap handle opt flagS filename)
	  do
	    start <- getCPUTime
	    wrap handle opt flagS filename
	    end <- getCPUTime
	    putStrLn ("CPU time: " ++ (show ((end - start) `div` 1000)) ++ "ns")
	False -> wrap handle opt flagS filename

checkType :: ValueContext -> String -> IO (Either TypeError (Src.Expr (Name, Type), Type))
checkType val_ctx s =
  do let parsed = reader s
     typeCheckWithEnv val_ctx parsed

printHelp :: IO ()
printHelp = do
	putStrLn ""
	putStrLn "Welcome to f2ji!"
	putStrLn "-----------------------------------------"
	putStrLn "[COMMANDS] [SOURCE FILE/FLAG]"
	putStrLn "Commands:"
	putStrLn ":help                 Print help manual"
	putStrLn ":run <sourceFile>     Compile and run sourceFile"
	putStrLn ":expr <sourceFile>    Show core expression of the file"
#ifdef Z3
	putStrLn ":se <sourceFile>      Symbolically evaluate the file"
#endif
	putStrLn ":let var = expr       Bind expr to var"
	putStrLn ":type var             Show the type of var"
	putStrLn ":replay               Replay all previous user commands"
	putStrLn ":clear                Clear environment"
	putStrLn ":quit                 Quit f2ji"
	putStrLn ""
	putStrLn "--- Commands for settings ---"
        putStrLn ":set method opt       Set compilation options"
        putStrLn ""
	putStrLn "--- Commands for displaying information ---"
	putStrLn ":show time on/off     Show/Hide execution time"
	putStrLn ":show file on/off     Show/Hide source file and .java file contents"
	putStrLn ":show <sourcefile>    Show file content"
	putStrLn ":show env             Show current bindings"
	putStrLn ":show method          Show available compilation options"
	putStrLn "-----------------------------------------"
	putStrLn ""
