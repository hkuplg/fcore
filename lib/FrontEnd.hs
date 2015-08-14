module FrontEnd (source2core) where

import Parser    (reader, P(..))
import TypeCheck (typeCheck)
import Desugar   (desugar)
import Simplify  (simplify)
import qualified OptiUtils (Exp(Hide))
import BackEnd   (DumpOption(..))

import qualified SystemFI as FI   (FExp(HideF), prettyExpr)

import System.Exit   (exitFailure)
import Text.PrettyPrint.ANSI.Leijen
import Control.Monad (when)

source2core :: DumpOption -> String -> IO OptiUtils.Exp
source2core optDump source
  = case reader source of
      PError msg -> do putStrLn msg
                       exitFailure
      POk parsed -> do
        when (optDump == Parsed) $ print (pretty parsed)
        result <- typeCheck parsed
        case result of
          Left typeError -> do print (pretty typeError)
                               exitFailure
          Right (_, checked) ->
            do when (optDump == TChecked) $ print (pretty checked)
               let fiExpr = desugar checked
               when (optDump == SystemFI) $ print (FI.prettyExpr fiExpr)
               return (OptiUtils.Hide (simplify (FI.HideF fiExpr)))
