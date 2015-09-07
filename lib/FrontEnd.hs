module FrontEnd (source2core) where

import           BackEnd (DumpOption(..))
import qualified Core
import           Desugar (desugar)
import qualified CoreNew
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

source2core :: DumpOption -> (FilePath, String) -> IO OptiUtils.Exp
source2core optDump (filePath, source)
  = case reader source of
      PError msg -> do putStrLn msg
                       exitFailure
      POk parsed -> do
        when (optDump == Parsed) $ print (pretty parsed)
        result <- typeCheck parsed
        case result of
          Left typeError ->
            do print (prettyProblems [makeProblem (filePath, source) typeError])
               exitFailure
          Right (_, checked) ->
            do when (optDump == TChecked) $ print (pretty checked)
               let fiExpr = desugar checked
               when (optDump == SystemFI) $ print (FI.prettyExpr fiExpr)
               return (OptiUtils.Hide (simplify (FI.HideF fiExpr)))

s2n :: String -> IO (CoreNew.Expr CoreNew.Index)
s2n source
  = case reader source of
      PError msg -> do putStrLn msg
                       exitFailure
      POk parsed -> do
        result <- typeCheck parsed
        case result of
          Left typeError -> exitFailure
          Right (_, checked) ->
            do let fiExpr = desugar checked
               let exp = simplify (FI.HideF fiExpr)
               putStrLn "-- Core:"
               print (Core.prettyExpr exp)
               let expcn = CoreNew.coreExprToNew exp
               putStrLn "\n-- New Core:"
               print (CoreNew.pretty expcn)
               putStrLn ""
               return expcn

testnc :: IO ()
testnc =
  do
    s2n "let rec fact (n : Int) : Int = if n == 0 then 1 else n * fact (n - 1); fact 5"
    s2n "let id[A] (x : A) = x; id [forall A. A -> A] id 10"
    s2n "let get [D, E, F] (x : E) (y : D) : D = y; get [Int, String, Int] \"abc\" 5"
    return ()

