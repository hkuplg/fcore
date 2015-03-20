{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Console.Haskeline		(runInputT, defaultSettings)
import System.IO
import System.Process hiding (runCommand)
import System.Directory			(doesFileExist)

import Control.Monad.Error

import Data.FileEmbed			(embedFile)
import qualified Data.Map as Map

import Translations
import JavaUtils
import StringPrefixes			(namespace)

import Loop
import qualified Environment as Env
import qualified History as Hist
import FileIO 				(TransMethod (Naive))
import qualified Data.ByteString as B
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

runtimeBytes :: B.ByteString
runtimeBytes = $(embedFile "runtime/runtime.jar")

writeRuntimeToTemp :: IO ()
writeRuntimeToTemp =
  do tempdir <- getTemporaryDirectory
     let tempFile = tempdir </> "runtime.jar"
     B.writeFile tempFile runtimeBytes

main :: IO ()
main = do
     writeRuntimeToTemp
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

printFile :: IO ()
printFile = do 
	f <- getLine
	contents <- readFile f
	putStr contents



