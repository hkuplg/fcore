module PrettyUtils where

import Text.PrettyPrint.Leijen
import Data.Char (ord, chr)

type PrecedenceEnv   = (PrecedenceLevel, PrecedenceDelta)
type PrecedenceLevel = Int

data PrecedenceDelta = PrecMinus | PrecPlus

basePrecEnv :: PrecedenceEnv
basePrecEnv = (0, PrecMinus)

parensIf :: PrecedenceEnv -> PrecedenceLevel -> Doc -> Doc
parensIf (envLevel, envDelta) myLevel doc
  | envLevel > myLevel  = parens doc
  | envLevel == myLevel =
    case envDelta of PrecPlus  -> parens doc
                     PrecMinus -> doc
  | otherwise = doc

nameTVar :: Int -> String
nameTVar = nameVarFrom 'A'

nameVar :: Int -> String
nameVar = nameVarFrom 'a'

nameVarFrom :: Char -> Int -> String
nameVarFrom c n
  | n < 26    = [chr (ord c + n)]
  | otherwise = c : show n
