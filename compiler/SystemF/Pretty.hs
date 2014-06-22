{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module SystemF.Pretty where

import qualified Language.Java.Syntax as JS
import qualified Language.Java.Pretty as JP (prettyPrint)
import Text.PrettyPrint
import Data.Char        (chr, ord)
import Data.List        (intersperse)

import SystemF.Syntax


prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

prettyPrintPFTyp :: PFTyp Int -> String
prettyPrintPFTyp = prettyPrint

prettyPrintPFExp :: PFExp Int Int -> String
prettyPrintPFExp = prettyPrint


parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
    | inheritedPrec <= 0          = t
    | inheritedPrec < currentPrec = parens t
    | otherwise                   = t

class Pretty a where
    pretty :: a -> Doc
    pretty = prettyPrec 0 (0, 0)
  
    prettyPrec :: Int -> (Int, Int) -> a -> Doc
    prettyPrec _ _ = pretty

instance Pretty (PFTyp Int) where
    prettyPrec p l@(ltvar, lvar) t = case t of
        FTVar a    -> text (tvar a)
        FForall f  -> text ("forall " ++ tvar ltvar ++ ".") <+> prettyPrec p (ltvar+1,  lvar) (f ltvar)
        FFun t1 t2 -> parenPrec p 2 $ prettyPrec 1 l t1 <+> text "->" <+> prettyPrec p l t2
        PFInt      -> text "Int"

instance Pretty (PFExp Int Int) where
    prettyPrec p l@(ltvar, lvar) e = case e of
        FVar x           -> text (var x)
        FLit n           -> if n < 0 then parenPrec p 5 (integer n) else integer n
        FTuple es        -> parens $ hcat $ intersperse comma $ map (prettyPrec p l) es

        FProj i e        -> parenPrec p 1 $ prettyPrec 1 l e <> text ("._" ++ show i)

        FApp e1 e2       -> parenPrec p 2 $ prettyPrec 2 l e1 <+> prettyPrec 1 l e2 
        FTApp e t        -> parenPrec p 2 $ prettyPrec 2 l e  <+> prettyPrec 1 l t

        FBLam f          -> parenPrec p 3 $ 
                                text ("/\\" ++ tvar ltvar ++ ".") 
                                <+> prettyPrec 0 (ltvar+1, lvar) (f ltvar)
        FLam t f         -> parenPrec p 3 $ 
                                text ("\\(" ++ var lvar ++ " : " ++ show (prettyPrec p (ltvar, lvar+1) t) ++ ").")
                                <+> prettyPrec 0 (ltvar, lvar+1) (f lvar)
        FFix t1 f t2     -> parenPrec p 3 $ 
                                text ("fix " ++ var lvar ++ ".")
                                <+> text ("\\(" ++ (var (lvar+1) ++ " : " ++ show (prettyPrec p (ltvar, lvar+2) t1)) ++ ").")
                                <+> prettyPrec 0 (ltvar, lvar+2) (f lvar (lvar+1)) <+> colon <+> prettyPrec 0 (ltvar, lvar+2) t2

        FPrimOp e1 op e2 -> parenPrec p q $ prettyPrec q l e1 <+> text (JP.prettyPrint op) <+> prettyPrec (q-1) l e2 
                                where q = opPrec op 

        Fif0 e1 e2 e3    -> text "if0" <+> prettyPrec p l e1 <+> text "then" <+> prettyPrec p l e2 <+> text "else" <+> prettyPrec p l e3

tvar :: Int -> String
tvar n
    | n < 0     = error "`var` called with n < 0"
    | n < 26    = [chr (ord 'A' + n)]
    | otherwise = "A" ++ show (n - 25)

var :: Int -> String
var n
    | n < 0     = error "`tvar` called with n < 0"
    | n < 26    = [chr (ord 'a' + n)]
    | otherwise = "a" ++ show (n - 25)

-- Precedence of operators based on the table in:
-- http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
opPrec JS.Mult    = 3
opPrec JS.Div     = 3
opPrec JS.Rem     = 3
opPrec JS.Add     = 4
opPrec JS.Sub     = 4
opPrec JS.LThan   = 6
opPrec JS.GThan   = 6
opPrec JS.LThanE  = 6
opPrec JS.GThanE  = 6
opPrec JS.Equal   = 7
opPrec JS.NotEq   = 7
opPrec JS.CAnd    = 11
opPrec JS.COr     = 12
opPrec op         = error $ "Something impossible happens! The operator '" 
                            ++ JP.prettyPrint op ++ "' is not part of the language."
