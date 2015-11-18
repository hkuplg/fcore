module FrontEnd (source2core) where

import           BackEnd (DumpOption(..))
import           Desugar (desugar)
import qualified OptiUtils (Exp(Hide))
import           Parser (reader, P(..))
import           Problem
import           Simplify (simplify)
import           SrcLoc
import qualified SystemFI as FI (FExp(HideF), prettyExpr)
import           TypeCheck (typeCheck)
import           TypeErrors

import           Control.Monad (when)
import           Data.List.Split (splitOn)
import           System.Exit (exitFailure)
import           Text.PrettyPrint.ANSI.Leijen hiding (line, column)

makeProblem :: (FilePath, String) -> LTypeErrorExpr -> Problem
makeProblem (filePath, source) (L loc (err, _maybeExpr))
  = Problem
    { problemLevel    = Error
    , problemLocation = (filePath, line loc, column loc)
    , problemLine     = splitOn "\n" source !! (line loc - 1)
    , problemDescription = pretty err
    }

source2core :: DumpOption -> String -> (FilePath, String) -> IO OptiUtils.Exp
source2core optDump rawMethods (filePath, source)
  = case reader source of
      PError msg -> do putStrLn msg
                       exitFailure
      POk parsed -> do
        when (optDump == Parsed) $ print (pretty parsed)
        result <- typeCheck rawMethods parsed
        case result of
          Left typeError ->
            do print (prettyProblems [makeProblem (filePath, source) typeError])
               exitFailure
          Right (_, checked) ->
            do when (optDump == TChecked) $ print (pretty checked)
               let fiExpr = desugar checked
               when (optDump == SystemFI) $ print (FI.prettyExpr fiExpr)
               return (OptiUtils.Hide (simplify (FI.HideF fiExpr)))
