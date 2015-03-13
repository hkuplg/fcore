{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  FileLoad
Description :  Testing framework for make test 2
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Boya Peng <u3502350@connect.hku.hk>
Stability   :  experimental
Portability :  portable

-}

module Main where

import System.IO
import System.Process
import System.Directory                 (doesFileExist, getDirectoryContents)
import System.FilePath                  (takeFileName)
import System.TimeIt        (timeIt)

import Data.Time            (getCurrentTime, diffUTCTime)
import Control.Monad        (when)

import Translations
import FileIO
import JavaUtils
import StringPrefixes       (namespace)
import qualified Data.ByteString as B
import Data.FileEmbed       (embedFile)
import System.Directory     (getTemporaryDirectory)
import System.FilePath      ((</>))

runtimeBytes :: B.ByteString
runtimeBytes = $(embedFile "runtime/runtime.jar")

writeRuntimeToTemp :: IO ()
writeRuntimeToTemp =
  do tempdir <- getTemporaryDirectory
     let tempFile = tempdir </> "runtime.jar"
     B.writeFile tempFile runtimeBytes

testCasesPath = "testsuite/tests/should_run/"

initReplEnv :: [TransMethod] -> Compilation -> [String] -> IO ()
initReplEnv method opt xs =  do
     cp <- getClassPath
     let p = (proc "java" ["-cp", cp, (namespace ++ "FileServer"), cp])
                  {std_in = CreatePipe, std_out = CreatePipe}
     (Just inP, Just outP, _, proch) <- createProcess p
     hSetBuffering inP NoBuffering
     hSetBuffering outP NoBuffering
     loadFile inP outP method opt xs
     terminateProcess proch

loadFile :: Handle -> Handle -> [TransMethod] -> Compilation -> [String] -> IO ()
loadFile inP outP method opt xs = do
    putStrLn "-------------------------------------"
    putStrLn ("Compileation Option: " ++ show (method))
    loadAll inP outP method opt xs

loadAll ::  Handle -> Handle -> [TransMethod] -> Compilation -> [String] -> IO ()
loadAll _ _ _ _ [] = return ()
loadAll inP outP method opt (x:xs) = do
    let compileOpt = (0, opt, method)
    let name = takeFileName x
    when (head name /= '.') $
     do putStrLn ("Running " ++ name)
        output <- getStandardOutput x
        putStrLn $ "\x1b[32m" ++ "Standard output: " ++ output
        wrap (inP, outP) (receiveMsg2 output) compileOpt True False x
    loadAll inP outP method opt xs

getStandardOutput :: FilePath -> IO String
getStandardOutput file = do
  content <- readFile file
  let func = unwords . tail . words
  return $ func ((lines content) !! 0)

fileExist :: String -> IO ()
fileExist name = do
        exist <- doesFileExist name
        if (exist)
          then return ()
          else fileExist name

addFilePath :: [FilePath] -> [FilePath]
addFilePath [] = []
addFilePath (x:xs) = (testCasesPath ++ x) : (addFilePath xs)

main :: IO ()
main = do
  writeRuntimeToTemp
  start <- getCurrentTime
  xs <- getDirectoryContents testCasesPath
  let (x:y:ys) = xs
  let zs = addFilePath ys
  putStrLn ""
  putStrLn $ "Running test from " ++ testCasesPath
  putStrLn "-------------------------------------"
  timeIt $ initReplEnv [Naive] compileN zs
  timeIt $ initReplEnv [Naive, Apply] compileAO zs
  timeIt $ initReplEnv [Naive, Stack] compileSN zs
  timeIt $ initReplEnv [Naive, Apply, Stack] compileS zs
  putStrLn "-------------------------------------"
  putStrLn "Finished!"
  end <- getCurrentTime
  putStrLn $ "Running Time " ++ show (diffUTCTime end start)
