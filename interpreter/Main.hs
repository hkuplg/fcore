{-# LANGUAGE TemplateHaskell #-}

module Main where

import BackEnd
import FileIO                           (TransMethod (Naive))
import Loop
import RuntimeProcessManager            (withRuntimeProcess)

import System.Console.Haskeline         (runInputT, defaultSettings)
import System.IO

import Data.FileEmbed                   (embedFile)
import qualified Data.Map as Map

import qualified Environment as Env
import qualified History as Hist
import qualified Data.ByteString as B
import System.Directory (doesFileExist, getTemporaryDirectory)
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
  withRuntimeProcess "FileServer" LineBuffering
       (\(inP,outP) ->
        do liftIO printHelp
           runInputT defaultSettings
                         (Loop.loop (inP, outP) (0, compileN, [Naive])
                          Map.empty Env.empty Hist.empty Hist.empty 0 False False False False 0))

fileExist :: String -> IO ()
fileExist name = do
        exist <- doesFileExist name
        unless exist $ fileExist name

printFile :: IO ()
printFile = do
        f <- getLine
        contents <- readFile f
        putStr contents



