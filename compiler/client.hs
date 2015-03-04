{- |
Module      :  Client
Description :  Controller for the REPL
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Boya Peng <u3502350@connect.hku.hk>
Stability   :  stable
Portability :  portable
-}

module Client where

import System.Console.Haskeline		(runInputT, defaultSettings)
import System.IO
import System.Process hiding (runCommand)
import System.Directory			(doesFileExist)

import Control.Monad.Error

import qualified Data.Map as Map

import Translations
import JavaUtils
import StringPrefixes			(namespace)

import Loop
import qualified Environment as Env
import qualified History as Hist
import FileIO 				(TransMethod (Naive))


main :: IO ()
main = do 
     cp <- getClassPath
     let p = (proc "java" ["-cp", cp, (namespace ++ "FileServer"), cp])
                  {std_in = CreatePipe, std_out = CreatePipe}
     (Just inP, Just outP, _, proch) <- createProcess p
     hSetBuffering inP LineBuffering
     hSetBuffering outP LineBuffering
     liftIO printHelp
     runInputT defaultSettings 
	       (Loop.loop (inP, outP) (0, compileN, [Naive]) 
		          Map.empty Env.empty Hist.empty Hist.empty 0 False False False False 0)
     terminateProcess proch
     
fileExist :: String -> IO ()
fileExist name = do
	exist <- doesFileExist name
	if (exist) 
	  then return ()
	  else fileExist name	

printFile = do 
	f <- getLine
	contents <- readFile f
	putStr contents
