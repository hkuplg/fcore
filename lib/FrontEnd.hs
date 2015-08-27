module FrontEnd (source2core) where

import qualified Config
import SrcLoc
import Parser    (reader, P(..))
import TypeCheck (typeCheck)
import TypeErrors
import Problem
import Desugar   (desugar)
import Simplify  (simplify)
import qualified OptiUtils (Exp(Hide))
import BackEnd   (DumpOption(..))

import qualified SystemFI as FI   (FExp(HideF), prettyExpr)

import Data.List.Split (splitOn)
import System.Exit   (exitFailure)
import Text.PrettyPrint.ANSI.Leijen hiding (line, column)
import Control.Monad (when)

makeProblem :: (FilePath, String) -> LTypeErrorExpr -> Problem
makeProblem (filePath, source) (L loc (err, _maybeExpr))
  = Problem
    { problemLevel    = Error
    , problemLocation = (filePath, line loc, column loc)
    , problemLine     = splitOn "\n" source !! (line loc - 1 - Config.lineNumberOffset)
    , problemDescription = pretty err
    }

source2core :: DumpOption -> (FilePath, String) -> IO OptiUtils.Exp
source2core optDump (filePath, source)
  = case reader source of
      PError msg -> do putStrLn msg
                       exitFailure
      POk parsed -> do
        when (optDump == DumpParsed) $ print (pretty parsed)
        result <- typeCheck parsed
        case result of
          Left typeError ->
            do print (prettyProblems [makeProblem (filePath, source) typeError])
               exitFailure
          Right (_, checked) ->
            do when (optDump == DumpTChecked) $ print (pretty checked)
               let fiExpr = desugar checked
               when (optDump == DumpCore) $ print (FI.prettyExpr fiExpr)
               return (OptiUtils.Hide (simplify (FI.HideF fiExpr)))
