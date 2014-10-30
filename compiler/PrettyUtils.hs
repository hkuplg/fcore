module PrettyUtils
  ( -- Type classes
    -- Outputable(..)

    PrecLevel
  , PrecDelta(..)
  , Prec
  , basePrec

  , parensIf

    -- Pretty printing combinators
  , arrow, forall, ampersand, lambda, biglambda, dcomma, unit

  , prettyTVar, prettyVar
  ) where

import Text.PrettyPrint.Leijen
import Data.Char (ord, chr)

-- class Outputable a where
--   pretty :: a -> Doc
--   pretty = prettyPrec basePrec

--   prettyPrec   :: Prec -> a -> Doc
--   prettyPrec _ = PrettyUtils.pretty

arrow, forall, ampersand, lambda, biglambda, dcomma, unit :: Doc
arrow     = text "->"
forall    = text "forall"
ampersand = text "&"
lambda    = text "\\"
biglambda = text "/\\"
dcomma    = text ",,"
unit      = text "()"

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

prettyTVar, prettyVar :: Int -> Doc
prettyTVar = prettyVarFrom 'A'
prettyVar  = prettyVarFrom 'a'

prettyVarFrom :: Char -> Int -> Doc
prettyVarFrom c n
  | n < 26    = text [chr (ord c + n)]
  | otherwise = text (c : show n)
