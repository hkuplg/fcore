module SpecHelper
  (discoverTestCases
  ,discoverTestCases'
  ) where

import System.Directory (getDirectoryContents)
import System.FilePath  (dropExtension, (</>))

import Control.Applicative ((<$>))
import Control.Monad       (forM)

import Data.Char (isSpace)
import Data.List (isSuffixOf)

type Name   = String
type Source = String
type ExpectedOutput = String

discoverTestCases :: FilePath -> IO [(Name, Source)]
discoverTestCases directory =
  do fileNames <- filter (isSuffixOf ".sf") <$> getDirectoryContents directory
     forM fileNames (\f ->
       do source <- readFile (directory </> f)
          return (dropExtension f, source))

discoverTestCases' :: FilePath -> IO [(Name, Source, ExpectedOutput)]
discoverTestCases' directory =
  do fileNames <- filter (isSuffixOf ".sf") <$> getDirectoryContents directory
     forM fileNames (\f ->
       do source <- readFile (directory </> f)
          let firstLine = takeWhile (/= '\n') source
          case firstLine of
            '-':'-':'>':_ -> return (dropExtension f, source, strip (drop 3 firstLine))
            _             -> error (directory </> f ++ ": " ++
                                    "The integration test file should start with '-->', \
                                    \followed by the expected output"))

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
