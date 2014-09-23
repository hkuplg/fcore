module PrettyUtils
  ( -- Type classes
    Outputable(..)

  , PrecLevel
  , PrecDelta(..)
  , Prec
  , basePrec

  , parensIf

    -- Pretty printing combinators
  , arrow, forall, ampersand, lambda, biglambda, dcomma

  , pprTVar, pprVar
  ) where

import Text.PrettyPrint.Leijen
import Data.Char (ord, chr)

class Outputable a where
  ppr :: a -> Doc
  ppr = pprPrec basePrec

  pprPrec   :: Prec -> a -> Doc
  pprPrec _ = ppr

arrow, forall, ampersand, lambda, biglambda, dcomma :: Doc
arrow     = text "->"
forall    = text "forall"
ampersand = text "&"
lambda    = text "\\"
biglambda = text "/\\"
dcomma    = text ",,"

type PrecLevel = Int
data PrecDelta = PrecMinus | PrecPlus
type Prec      = (PrecLevel, PrecDelta)

basePrec :: Prec
basePrec = (0, PrecMinus)

parensIf :: Prec -> PrecLevel -> Doc -> Doc
parensIf (envLevel, envDelta) myLevel doc
  | envLevel > myLevel  = parens doc
  | envLevel == myLevel =
    case envDelta of
      PrecPlus  -> parens doc
      PrecMinus -> doc
  | otherwise           = doc

pprTVar, pprVar :: Int -> Doc
pprTVar = pprVarFrom 'A'
pprVar  = pprVarFrom 'a'

pprVarFrom :: Char -> Int -> Doc
pprVarFrom c n
  | n < 26    = text [chr (ord c + n)]
  | otherwise = text (c : show n)