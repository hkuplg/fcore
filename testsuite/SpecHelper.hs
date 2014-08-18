module SpecHelper (discoverTestCases) where

import System.Directory (getDirectoryContents)
import System.FilePath  (dropExtension, (</>))

import Control.Applicative ((<$>))
import Control.Monad       (forM)

import Data.List (isSuffixOf)

type Name   = String
type Source = String
type TestCase = (Name, Source)

discoverTestCases :: FilePath -> IO [TestCase]
discoverTestCases directory =
  do fileNames <- filter (isSuffixOf ".sf") <$> getDirectoryContents directory
     forM fileNames (\f ->
       do source <- readFile (directory </> f)
          return (dropExtension f, source))