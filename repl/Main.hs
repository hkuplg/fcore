module Main where


import           Control.Monad
import           Control.Monad.IO.Class
import           FileIO (TransMethod (Naive))
import           RuntimeProcessManager (withRuntimeProcess)
import           System.Console.Haskeline (runInputT, defaultSettings)
import           System.Directory (doesFileExist)
import           System.IO
import qualified Data.Map as Map

-- REPL-specific modules
import           BackEnd
import           Loop
import qualified Environment as Env
import qualified History as Hist

main :: IO ()
main = do
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
