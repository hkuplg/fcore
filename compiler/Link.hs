module Link where 

import System.IO
import System.FilePath	(dropExtension)
import Data.List.Split	(splitOn)

link :: FilePath -> String -> IO ()
link file mod = do
  let list = lines mod
  let contM = concatenate (take ((length list) - 1) list)
  contF <- readFile file
  let newFile = (dropExtension file) ++ "c.sf"
  writeFile newFile ("let\n" ++ contM ++ "in\n")
  appendFile newFile contF

linkModule :: [FilePath] -> IO String
linkModule [] = return ""
linkModule (x:xs) = do
  contM <- readFile x
  let content = breakLine contM
  rest <- linkModule xs
  return (content ++ rest)

breakLine :: String -> String
breakLine content = let list = lines content in addByLine list

addByLine :: [String] -> String
addByLine [] = ""
addByLine (x:xs) = x ++ "\nand\n" ++ (addByLine xs) 

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
  
