module Main where

import System.Console.Haskeline
import System.IO
import System.Process hiding (runCommand)
import Control.Monad.Error
import Data.Char
import Data.List.Split
import Data.List
import Translations

-- proc :: FileName -> [String] -> CreateProcess
-- FileName is the command you want to execute, [String] are all the arguments followed


main = 
  do let p = (proc "java" ["-cp", "runtime.jar:.", "FileServer"])
                  {std_in = CreatePipe, std_out = CreatePipe}
     (Just inP, Just outP, _, proch) <- createProcess p
     hSetBuffering inP LineBuffering
     hSetBuffering outP LineBuffering
     runInputT defaultSettings (loop inP outP)
     
loop :: Handle -> Handle -> InputT IO ()
loop inP outP = do
	msg <- getInputLine "% "
	case msg of
	  Nothing -> return ()
	  Just input -> do let command = getCommand input
			   runCommand inP outP command input
	
--liftIO :: IO () -> InputT IO ()
{-`InputT` is an extra layer to work through and so need to *lift* your `IO ()` value
into the `InputT IO` layer-}

getCommand :: String -> String
getCommand msg = let (x : xs) = splitOn " " msg  in x

runCommand :: Handle -> Handle -> String -> String -> InputT IO ()
runCommand inP outP command msg = do
	case (stripPrefix ":" command) of
	  Just "quit" -> return ()
	  Just "send" -> do let name = getFileName msg
	  		    case name of
			      "" ->  outputStrLn "Illegal input!"
			      fileName -> liftIO (wrap inP outP fileName)
	  		    loop inP outP
	  Just input -> do outputStrLn $ "Command not recognized: " ++ input
	  	           loop inP outP
	  Nothing -> do liftIO (writeFile "main.sf" msg)
	  		liftIO (wrap inP outP "main.sf")
	  	        loop inP outP

getFileName :: String -> String
getFileName msg = case length (splitOn " " msg) of
		    2 -> let [x, y] = splitOn " " msg in y
		    n -> "" 

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
	javaFile <- sf2java False compileAO className contents
	let file = javaFile ++ "\n" ++  "//end of file"
	putStrLn file
	sendFile h file
	putStrLn "sent!"

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

--printFile :: FilePath -> IO()
printFile = do 
	f <- getLine
	contents <- readFile f
	putStr contents


