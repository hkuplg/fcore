module SpecHelper
  (discoverTestCases
  ,parseExpectedOutput
  ) where

import System.Directory (getDirectoryContents)
import System.FilePath  (dropExtension, (</>))

import Control.Applicative ((<$>))

import Data.Char (isSpace)
import Data.List (isSuffixOf)

type Name   = String
type Source = String
type ExpectedOutput = String

discoverTestCases :: FilePath -> IO [(Name, FilePath)]
discoverTestCases directory =
  do fileNames <- filter (isSuffixOf ".sf") <$> getDirectoryContents directory
     return (map (\f -> (dropExtension f, directory </> f)) fileNames)

parseExpectedOutput :: Source -> Maybe ExpectedOutput
parseExpectedOutput source =
  let firstLine = takeWhile (/= '\n') source in
  case firstLine of
    '-':'-':'>':_ -> Just (strip (drop 3 firstLine))
    _             -> Nothing

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
