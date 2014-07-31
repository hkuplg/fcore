module Language.ESF where

import Language.ESF.Syntax
import Language.ESF.Parser
import Language.ESF.TypeCheck

toptype :: String -> String
toptype s =
  case infer (reader s) of
    Nothing -> "Typecheck failed"
    Just t  -> prettyShow t
