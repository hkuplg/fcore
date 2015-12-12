{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Lab where

import qualified Src
import qualified Core
import Parser

import System.Directory

filePath = "examples/declsyntax/EvenOdd.sf"

main = do source <- readFile filePath
          print (parseProgram source)
