{- |
Module      :  FileLoad 
Description :  Perform IO for the REPL
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Boya Peng <u3502350@connect.hku.hk>
Stability   :  stable
Portability :  portable
-}

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
import JavaUtils (inferClassName, inferOutputPath)

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

wrap :: Connection -> (Int -> Handle -> IO Int) -> CompileOpt -> Bool -> Bool -> String -> IO Int 
wrap (inP, outP) receiveMsg opt flagC flagS name = do
	exist <- doesFileExist name
	if not exist
	  then do
	    putStrLn $ name ++ " does not exist"
	    return 1
  	  else do
	    correct <- send inP opt flagC flagS name
	    case correct of 
	      True  -> receiveMsg 0 outP
	      False -> return 1

getClassName :: String -> String
getClassName (x : xs) = (toUpper x) : xs

send :: Handle -> CompileOpt -> Bool -> Bool -> FilePath -> IO Bool 
send h (n, opt, method) flagC flagS f = do 
  contents <- readFile f
  -- let path = dropFileName f
  let className = inferClassName . inferOutputPath $ f
  result <- E.try (sf2java False NoDump opt className contents)
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
	
receiveMsg :: Int -> Handle -> IO Int 
receiveMsg error h = do
  msg <- hGetLine h
  if msg == "exit" 
    then return error
    else do putStrLn msg
            receiveMsg error h

-- make test2
receiveMsg2 :: String -> Int -> Handle -> IO Int
receiveMsg2 output error h = do
  msg <- hGetLine h
  if msg == "exit"
    then return error
    else do 
      if msg /= output 
        then do putStrLn $ "\x1b[31m" ++ "Incorrect: " ++ msg
                putStrLn "\x1b[0m"
                receiveMsg2 output (error+1) h
        else do putStrLn $ "\x1b[32m" ++ "Correct: " ++ msg
                putStrLn "\x1b[0m"
                receiveMsg2 output error h

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

