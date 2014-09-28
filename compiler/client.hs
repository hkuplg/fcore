module Main where

import System.Console.Haskeline
import System.IO
import System.Process
import Control.Monad.Error
import Data.Char
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
     where
     	loop :: Handle -> Handle -> InputT IO ()
  	loop inP outP = do
		msg <- getInputLine "% "
		case msg of
		  Nothing -> return ()
  		  Just "quit" -> return ()
		  Just "send" -> do liftIO (wrap inP outP)
		  		    loop inP outP
		  Just input -> do outputStrLn $ "Command not regonized: " ++ input
		  		   loop inP outP
--liftIO :: IO () -> InputT IO ()
{-`InputT` is an extra layer to work through and so need to *lift* your `IO ()` value
into the `InputT IO` layer-}

wrap :: Handle -> Handle -> IO String
wrap inP outP = do
	send inP
	receiveMsg outP

send :: Handle -> IO () 
send h = do 
	putStr "File name: "
	msg <- getLine
	let className = getClassName msg
	sendMsg h (className ++ ".java")
	sfToJava h msg

getClassName :: String -> String
getClassName (x : xs) = (toUpper x) : (takeWhile (/= '.') xs)
--if (x == '.') then "" else [x] ++ getClassName xs

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

receiveMsg :: Handle -> IO String 
receiveMsg h = do
	msg <- hGetLine h
	if msg == "exit" 
	then return "" 
	else do putStrLn msg
       		s <- receiveMsg h
		return $ msg ++ s

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


