{-# LANGUAGE ScopedTypeVariables
           , DeriveDataTypeable #-}

module FileIO where

import System.IO
import System.Process hiding  (runCommand)
import System.Directory       (removeFile, doesFileExist)
import System.FilePath        (dropExtension, dropFileName, takeFileName)

import qualified Control.Exception as E
import Control.Monad          (when)

import Data.Char
import Data.List.Split
import Data.List
import Data.Data
import Data.Typeable

import Translations

data TransMethod = Apply
                 | Naive
                 | Stack
                 | Unbox
                 | StackAU1
                 | StackAU2
                 | BenchN
                 | BenchS
                 | BenchNA
                 | BenchSA
                 | BenchSAI1
                 | BenchSAI2
                 deriving (Eq, Show, Data, Typeable, Ord)

type Connection = (Handle, Handle)
type CompileOpt = (Int, Compilation, [TransMethod])

wrap :: Connection -> (Handle -> IO ()) -> CompileOpt -> Bool -> Bool -> String -> IO ()
wrap (inP, outP) receiveMsg opt flagC flagS name = do
	exist <- doesFileExist name
	if not exist
	  then do
	    putStrLn $ name ++ " does not exist"
	    return ()
  	  else do
	    correct <- send inP opt flagC flagS name
	    case correct of 
	      True  -> receiveMsg outP
	      False -> return ()

getClassName :: String -> String
getClassName (x : xs) = (toUpper x) : xs

send :: Handle -> CompileOpt -> Bool -> Bool -> FilePath -> IO Bool 
send h (n, opt, method) flagC flagS f = do 
  contents <- readFile f
  let path = dropFileName f
  let className = getClassName $ dropExtension (takeFileName f)
  result <- E.try (sf2java n NoDump opt className contents)
  case result of 
    Left  (_ :: E.SomeException) -> 
      do putStrLn ("\x1b[31m" ++ "invalid expression sf2Java")
         putStrLn "\x1b[0m"
         return False
    Right javaFile	             ->
      do sendMsg h (className ++ ".java")
         let file = javaFile ++ "\n" ++  "//end of file"
         sendFile h file
         when flagS $  
           do putStrLn contents
              putStrLn file
         return True
	
receiveMsg :: Handle -> IO () 
receiveMsg h = do
  msg <- hGetLine h
  if msg == "exit" 
    then return ()
    else do putStrLn msg
            receiveMsg h

-- make test2
receiveMsg2 :: String -> Handle -> IO ()
receiveMsg2 output h = do
  msg <- hGetLine h
  if msg == "exit"
    then return ()
    else do 
      if msg /= output 
        then putStrLn $ "\x1b[31m" ++ "Incorrect: " ++ msg
        else putStrLn $ "\x1b[32m" ++ "Correct: " ++ msg
      putStrLn "\x1b[0m"
      receiveMsg2 output h

-- make test
receiveMsg3 outP = do
  msg <- hGetLine outP
  if msg == "exit" then receiveMsg3 outP else return msg

sendMsg :: Handle -> String -> IO ()
sendMsg h msg = do 
  hPutStrLn h msg

sendFile :: Handle -> String -> IO ()
sendFile h f = do
  hPutStrLn h f

