{-# LANGUAGE TemplateHaskell #-}

module Client where

import System.Console.Haskeline
import System.IO
import System.Process hiding (runCommand)
import System.TimeIt
import System.Directory			(removeFile, doesFileExist)

import Control.Monad.Error

import Data.Char
import Data.List.Split
import Data.List
import Data.FileEmbed			(embedFile)
import qualified Data.ByteString 	(ByteString, writeFile)

import Translations
import JavaUtils
import ParseCMD
--import ReplCMD

runtimeBytes :: Data.ByteString.ByteString
runtimeBytes = $(embedFile "../runtime/runtime.jar")

main :: IO ()
main = do 
     exists <- doesFileExist =<< getRuntimeJarPath
     existsCur <- doesFileExist "./runtime.jar"
     unless (exists || existsCur) $ Data.ByteString.writeFile "./runtime.jar" runtimeBytes 
     fileExist "runtime.jar"
     let p0 = (proc "javac" ["-cp", "runtime.jar:.", "FileServer.java"])
     createProcess p0
     fileExist "FileServer.class"
     let p = (proc "java" ["-cp", "runtime.jar:.", "FileServer"])
                  {std_in = CreatePipe, std_out = CreatePipe}
     (Just inP, Just outP, _, proch) <- createProcess p
     hSetBuffering inP LineBuffering
     hSetBuffering outP LineBuffering
     liftIO printHelp
     runInputT defaultSettings (loop inP outP False False 0)
     
fileExist :: String -> IO ()
fileExist name = do
	exist <- doesFileExist name
	if (exist) 
	  then return ()
	  else fileExist name	

loop :: Handle -> Handle -> Bool -> Bool -> Int -> InputT IO ()
loop inP outP flagT flagS num = do
	msg <- getInputLine "% "
	case msg of
	  Nothing -> return ()
	  Just input -> runCommand inP outP flagT flagS num input
	
runCommand :: Handle -> Handle -> Bool -> Bool -> Int -> String -> InputT IO ()
runCommand inP outP flagT flagS num msg = do
	case (stripPrefix ":" msg) of
	  Just input -> 
		case parseMsg msg of
		  Left err ->  outputStrLn "Parse error"
		  Right line -> processCMD inP outP flagT flagS num line
	  Nothing -> do 
	  	let fileName = "main_" ++ (show num) ++ ".sf"
	  	liftIO (writeFile fileName msg)
	  	case flagT of 
		  True 	-> liftIO (timeIt (wrap inP outP flagS fileName))
		  False -> liftIO (wrap inP outP flagS fileName)
		liftIO (removeFile fileName)
		loop inP outP flagT flagS (num+1)

processCMD :: Handle -> Handle -> Bool -> Bool -> Int -> [String] -> InputT IO ()
processCMD inP outP flagT flagS num (x : xs) = do
	case x of 
	  ":help" -> liftIO printHelp
	  ":send" -> do
		case getCMD xs of
		  Just filename -> do 
	      		case flagT of
			  True	-> liftIO (timeIt (wrap inP outP flagS filename))
	  		  False -> liftIO (wrap inP outP flagS filename)
		  Nothing       ->  outputStrLn "Error input"
		loop inP outP flagT flagS num
	  ":time" -> case getCMD xs of 
		Just "on"  -> loop inP outP True flagS num
		Just "off" -> loop inP outP False flagS num
		_          -> do outputStrLn "Error input"
			         loop inP outP flagT flagS num
	  ":quit" -> return ()
	  ":showfile" -> case getCMD xs of
	  	Just "on" -> loop inP outP flagT True num
		Just "off" -> loop inP outP flagT False num
	  input	  -> do outputStrLn $ "Command not recognized: " ++ input
	  	        loop inP outP flagT flagS num

getCMD :: [String] -> Maybe String
getCMD xs = case xs of
		[x] -> Just x
		xs  -> Nothing

wrap :: Handle -> Handle -> Bool -> String -> IO ()
wrap inP outP flagS name = do
	send inP flagS name 
	exist <- doesFileExist name
	if exist
	  then receiveMsg outP
	  else return ()

send :: Handle -> Bool -> String -> IO () 
send h flagS name = do 
	exist <- doesFileExist name
	if not exist 
	  then do
	    putStrLn (name ++ " does not exist")
	    return ()
	  else do
	    let className = getClassName name
	    sendMsg h (className ++ ".java")
	    putStrLn name
	    sfToJava h flagS name

getClassName :: String -> String
getClassName (x : xs) = (toUpper x) : (takeWhile (/= '.') xs)

sfToJava :: Handle -> Bool -> FilePath -> IO ()
sfToJava h flagS f = do 
	contents <- readFile f
	let className = getClassName f
	javaFile <- sf2java 0 False compileAO className contents
	let file = javaFile ++ "\n" ++  "//end of file"
	sendFile h file
	case flagS of 
	  True -> do putStrLn contents
	  	     putStrLn file
	  False -> return ()
	--putStrLn "sent!"

receiveMsg :: Handle -> IO () 
receiveMsg h = do
	msg <- hGetLine h
	if msg == "exit" 
	  then return () 
	  else do putStrLn msg
       		  s <- receiveMsg h
		  return ()

sendMsg :: Handle -> String -> IO ()
sendMsg h msg = do 
	hPutStrLn h msg

sendFile :: Handle -> String -> IO ()
sendFile h f = do
	hPutStrLn h f

printHelp :: IO ()
printHelp = do
	putStrLn ""
	putStrLn "Welcome to f2ji!"
	putStrLn "[COMMANDS] [SOURCE FILE/FLAG]"
	putStrLn "Options:"
	putStrLn "  :help               Print help manual"
	putStrLn "  :send sourceFile    Load sourceFile"
	putStrLn "  :showfile on        Show source file and .java file contents"
	putStrLn "  :showfile off       Hide source file and .java file contents"
	putStrLn "  :time on            Show execution time"
	putStrLn "  :time off           Hide execution time"
	putStrLn "  :quit               Quit f2ji"

printFile = do 
	f <- getLine
	contents <- readFile f
	putStr contents



