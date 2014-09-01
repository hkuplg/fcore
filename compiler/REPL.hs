module Main (main) where

import Src.Syntax
import Src.Parser       (reader)
import Src.TypeCheck
import Src.Translation  (transTcExpr)

import Core

import Control.Monad    (forever)

import System.IO        (hFlush, stdout)
import Text.PrettyPrint.Leijen

main :: IO ()
main = repl

repl :: IO ()
repl = forever $ do
  putStr "> "; hFlush stdout
  s <- getLine
  let esf = reader s
  case infer esf of
    Left err -> print (pretty err)
    Right t  -> do
      print (pretty esf </> text "::" <+> pretty t)
      let sf = transTcExpr esf :: PFExp Int Int
      putStrLn $ SystemF.Pretty.prettyPrint sf
