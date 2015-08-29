{- |
Module      :  FileIO
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

import qualified Control.Exception as E
import           Control.Monad (when)
import           Data.Data
import           System.Directory (doesFileExist)
import           System.IO

import           BackEnd
import           FrontEnd
import           JavaUtils (inferClassName, inferOutputPath)

data TransMethod = Apply
                 | Naive
                 | SNaive
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
wrap (inP, outP) receMsg opt flagC flagS name = do
        exist <- doesFileExist name
        if not exist
          then do
            putStrLn $ name ++ " does not exist"
            return 1
          else do
            correct <- send inP opt flagC flagS name
            case correct of
              True  -> receMsg 0 outP
              False -> return 1

source2java :: Bool -> Bool -> DumpOption -> Compilation -> String -> (FilePath, String) -> IO String
source2java supernaive optInline optDump compilation className  (filePath, source) =
  do coreExpr <- source2core optDump (filePath, source)
     core2java supernaive optInline optDump compilation className coreExpr

send :: Handle -> CompileOpt -> Bool -> Bool -> FilePath -> IO Bool
send h (n, opt, method) flagC flagS f = do
  contents <- readFile f
  -- let path = dropFileName f
  let className = inferClassName . inferOutputPath $ f
  result <- E.try (if method == [SNaive]
                   then source2java True False NoDump opt className (f, contents)
                   else source2java False False NoDump opt className (f, contents))
  case result of
    Left  (_ :: E.SomeException) ->
      do putStrLn ("\x1b[31m" ++ "invalid expression sf2Java")
         putStrLn "\x1b[0m"
         return False
    Right javaFile                   ->
      do sendMsg h (className ++ ".java")
         let file = javaFile ++ "\n" ++  "//end of file"
         sendFile h file
         when flagS $
           do putStrLn contents
              putStrLn file
         return True

receiveMsg :: Int -> Handle -> IO Int
receiveMsg err h = do
  msg <- hGetLine h
  if msg == "exit"
    then return err
    else do putStrLn msg
            receiveMsg err h

sendMsg :: Handle -> String -> IO ()
sendMsg h msg = do
  hPutStrLn h msg

sendFile :: Handle -> String -> IO ()
sendFile h f = do
  hPutStrLn h f
