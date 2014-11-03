{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Console.Haskeline		(runInputT, defaultSettings)
import System.IO
import System.Process hiding (runCommand)
import System.Directory			(removeFile, doesFileExist)

import Control.Monad.Error

import Data.FileEmbed			(embedFile)
import qualified Data.ByteString 	(ByteString, writeFile)
import qualified Data.Map as Map

import Translations
import JavaUtils
import StringPrefixes			(namespace)

import Loop
import qualified Environment as Env
import qualified History as Hist

runtimeBytes :: Data.ByteString.ByteString
runtimeBytes = $(embedFile "runtime/runtime.jar")

main :: IO ()
main = do 
     exists <- doesFileExist =<< getRuntimeJarPath
     existsCur <- doesFileExist "./runtime.jar"
     unless (exists || existsCur) $ Data.ByteString.writeFile "./runtime.jar" runtimeBytes 
     fileExist "runtime.jar"
     --let p0 = (proc "javac" ["-cp", "runtime.jar:.", "FileServer.java"])
     --createProcess p0
     --fileExist "FileServer.class"
     let p = (proc "java" ["-cp", "runtime.jar:.", (namespace ++ "FileServer")])
                  {std_in = CreatePipe, std_out = CreatePipe}
     (Just inP, Just outP, _, proch) <- createProcess p
     hSetBuffering inP LineBuffering
     hSetBuffering outP LineBuffering
     liftIO printHelp
     runInputT defaultSettings 
	       (Loop.loop (inP, outP) (0, compileAO) Map.empty Env.empty Hist.empty 0 
		False False False 0)
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



