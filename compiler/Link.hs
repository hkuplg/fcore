module Link where 

import System.IO
import System.FilePath	(dropExtension)
import Data.List.Split	(splitOn)

link :: FilePath -> String -> IO ()
link file mod = do
  --let list = lines mod
  --let contM = concatenate (take ((length list) - 1) list)
  contF <- readFile file
  let newFile = (dropExtension file) ++ "c.sf"
  --writeFile newFile ("let\n" ++ contM ++ "in\n")
  writeFile newFile mod
  appendFile newFile contF

linkModule :: [FilePath] -> IO String
linkModule [] = return ""
linkModule (x:xs) = do
  contM <- readFile x
  --let content = breakLine contM (dropExtension x)
  rest <- linkModule xs
  --return (content ++ rest)
  return (contM ++ rest)

breakLine :: String -> String -> String
breakLine content name = let list = lines content in addByLine list name

addByLine :: [String] -> String -> String
addByLine [] _ = ""
addByLine (x:xs) name = name ++ "_" ++ x ++ "\nand\n" ++ (addByLine xs name) 

concatenate :: [String] -> String
concatenate [] = ""
concatenate (x:xs) = x ++ "\n" ++ concatenate xs

main = do
  putStr "File: "
  file <- getLine
  putStr "Modules: "
  mod <- getLine
  let modList = splitOn " " mod
  content <- linkModule modList
  link file content  
  
