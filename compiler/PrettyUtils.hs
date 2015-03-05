{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- |
Module      :  PrettyUtils
Description :  Utilities for pretty printing.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module PrettyUtils where

import Text.PrettyPrint.ANSI.Leijen
import Data.Char (ord, chr)
import Data.List (intersperse)

-- class Outputable a where
--   pretty :: a -> Doc
--   pretty = prettyPrec basePrec

--   prettyPrec   :: Prec -> a -> Doc
--   prettyPrec _ = PrettyUtils.pretty

arrow     = text "->"
forall    = text "forall"
ampersand = text "&"
lambda    = text "\\"
biglambda = text "/\\"
dcomma    = text ",,"
unit      = text "()"
bar       = text "|"
evalTo    = text "==>"

bquote :: Doc
bquote = char '`'

bquotes :: Doc -> Doc
bquotes = enclose bquote bquote

commas :: [Doc] -> Doc
commas docs = hcat $ intersperse (comma <> space) docs

prettyError :: Doc
prettyError = (bold . dullred) (text "error" <> colon)

-- | Prettify a document as code.
code :: Doc -> Doc
code = bold


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

prettyNVar :: String -> Int -> Doc
prettyNVar n j = prettyVar j <> (if n == "_" then empty else char '/' <> text n)

prettyVarFrom :: Char -> Int -> Doc
prettyVarFrom c n
  | n < 26    = text [chr (ord c + n)]
  | otherwise = text (c : show n)

intersperseBar :: [Doc] -> Doc
intersperseBar = foldl1 (\acc x -> acc <$$> bar <+> x)

prependNot :: Doc -> Doc
prependNot d = text "not" <+> d

combineWithAnd :: Doc -> Doc -> Doc
combineWithAnd d1 d2 = d1 <+> text "&&" <+> d2
