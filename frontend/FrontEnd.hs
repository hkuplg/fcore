module FrontEnd (source2core) where

import           BackEnd (DumpOption(..))
import           Desugar (desugar)
import qualified OptiUtils (Exp(Hide))
import           Parser (parseExpr, P(..))
import           Problem
import           Simplify (simplify)
import           SrcLoc
import qualified SystemFI as FI (FExp(HideF), prettyExpr)
import           TypeCheck (checkExpr)
import           TypeErrors

import           Control.Monad (when)
import           Data.List.Split (splitOn)
import           System.Exit (exitFailure)
import           Text.PrettyPrint.ANSI.Leijen hiding (line, column)

makeProblem :: (FilePath, String) -> Located TypeError -> Problem
makeProblem (filePath, source) (L loc err)
  = Problem
    { problemLevel    = Error
    , problemLocation = (filePath, line loc, column loc)
    , problemLine     = splitOn "\n" source !! (line loc - 1)
    , problemDescription = pretty err
    }

source2core :: DumpOption -> (FilePath, String) -> IO OptiUtils.Exp
source2core optDump (filePath, source)
  = case parseExpr source of
      ParseError msg -> do putStrLn msg
                           exitFailure
      ParseOk parsed -> do
        when (optDump == Parsed) $ print (pretty parsed)
        typeCheckResult <- checkExpr parsed
        case typeCheckResult of
          Left typeError ->
            do print (prettyProblems [makeProblem (filePath, source) typeError])
               exitFailure
          Right (_, checked) ->
            do when (optDump == TChecked) $ print (pretty checked)
               let fiExpr = desugar checked
               when (optDump == SystemFI) $ print (FI.prettyExpr fiExpr)
               return (OptiUtils.Hide (simplify (FI.HideF fiExpr)))
