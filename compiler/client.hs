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

runtimeBytes :: Data.ByteString.ByteString
runtimeBytes = $(embedFile "../runtime/runtime.jar")


main = 
  do exists <- doesFileExist =<< getRuntimeJarPath
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
     runInputT defaultSettings (loop inP outP False 0)
     
fileExist :: String -> IO ()
fileExist name = do
	exist <- doesFileExist name
	if (exist) 
	  then return ()
	  else fileExist name	

loop :: Handle -> Handle -> Bool -> Int -> InputT IO ()
loop inP outP flag num = do
	msg <- getInputLine "% "
	case msg of
	  Nothing -> return ()
	  Just input -> runCommand inP outP flag num input
	
runCommand :: Handle -> Handle -> Bool -> Int -> String -> InputT IO ()
runCommand inP outP flag num msg = do
	case (stripPrefix ":" msg) of
	  Just input -> 
		case parseMsg msg of
		  Left err ->  outputStrLn "Parse error"
		  Right line -> processCMD inP outP flag num line
	  Nothing -> do 
	  	let fileName = "main_" ++ (show num) ++ ".sf"
	  	liftIO (writeFile fileName msg)
	  	case flag of 
		  True 	-> liftIO (timeIt (wrap inP outP fileName))
		  False -> liftIO (wrap inP outP fileName)
		liftIO (removeFile fileName)
		loop inP outP flag (num+1)

processCMD :: Handle -> Handle -> Bool -> Int -> [String] -> InputT IO ()
processCMD inP outP flag num (x : xs) = do
	case x of 
	  ":send" -> do
		case getCMD xs of
		  Just filename -> do 
	      		case flag of
			  True	-> liftIO (timeIt (wrap inP outP filename))
	  		  False -> liftIO (wrap inP outP filename)
		  Nothing       ->  outputStrLn "Error input"
		loop inP outP flag num
	  ":time" -> case getCMD xs of 
		Just "on"  -> loop inP outP True num
		Just "off" -> loop inP outP False num
		_          -> do outputStrLn "Error input"
			         loop inP outP flag num
	  ":quit" -> return ()
	  input	  -> do outputStrLn $ "Command not recognized: " ++ input
	  	        loop inP outP flag num

getCMD :: [String] -> Maybe String
getCMD xs = case xs of
		[x] -> Just x
		xs  -> Nothing

wrap :: Handle -> Handle -> String -> IO ()
wrap inP outP name = do
	send inP name 
	receiveMsg outP

send :: Handle -> String -> IO () 
send h name = do 
	let className = getClassName name
	sendMsg h (className ++ ".java")
	sfToJava h name

getClassName :: String -> String
getClassName (x : xs) = (toUpper x) : (takeWhile (/= '.') xs)

sfToJava :: Handle -> FilePath -> IO ()
sfToJava h f = do 
	contents <- readFile f
	putStrLn contents
	let className = getClassName f
	javaFile <- sf2java 0 False compileAO className contents
	let file = javaFile ++ "\n" ++  "//end of file"
	putStrLn file
	sendFile h file
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

printFile = do 
	f <- getLine
	contents <- readFile f
	putStr contents


