module FileLoad where

import System.IO
import System.Process

import Data.Char
import Data.List

import Client
import JavaUtils

initReplEnv ::[String] -> IO ()
initReplEnv xs =  do 
     --cp <- getClassPath
     let p = (proc "java" ["-cp", "runtime.jar:.", "FileServer"])
               { std_in = CreatePipe, std_out = CreatePipe }
     (Just inP, Just outP, _, proch) <- createProcess p
     hSetBuffering inP NoBuffering
     hSetBuffering outP NoBuffering
     loadFile inP outP xs
     terminateProcess proch

loadFile :: Handle -> Handle -> [String] -> IO ()
loadFile _ _ [] = return () 
loadFile inP outP (x:xs) = do
    send inP False x
    receiveMsg outP
    loadFile inP outP xs 


 
