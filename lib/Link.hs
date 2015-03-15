module Link where 

import System.IO
import System.FilePath	(dropExtension)
import Data.Char
-- import Data.List.Split	(splitOn)

link :: FilePath -> String -> IO ()
link file mod = do
  --let list = lines mod
  --let contM = concatenate (take ((length list) - 1) list)
  content <- readFile file
  let contF = namespace content
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

namespace :: String -> String
namespace x = let ys = lines x in (unlines . replaceDot) ys

replaceDot :: [String] -> [String]
replaceDot [] = []
replaceDot (x:xs) = 
  let func = unwords . map checkDot in func (words x) : replaceDot xs

checkDot :: [Char] -> String
checkDot [] = []
checkDot [x] = [x]
checkDot (x:y:xs) = if x == '.' && isUpper y 
                       then ('_' : y : checkDot xs) 
                       else (x : checkDot (y:xs)) 

test :: FilePath -> IO String
test file = do
  content <- readFile file
  return $ namespace content

{-main = do
  putStr "File: "
  file <- getLine
  putStr "Modules: "
  mod <- getLine
  let modList = splitOn " " mod
  content <- linkModule modList
  link file content-}  
  
