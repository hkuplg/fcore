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

-- The following two are just monomorphic versions of `prettyPrint`.
prettyPrintPFTyp :: PFTyp Int -> String
prettyPrintPFTyp = prettyPrint

prettyPrintPFExp :: PFExp Int Int -> String
prettyPrintPFExp = prettyPrint

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
    | inheritedPrec <= 0          = t
    | inheritedPrec < currentPrec = parens t
    | otherwise                   = t

type Label = Int

class Pretty a where
    pretty :: a -> Doc
    pretty = prettyPrec 0 (0, 0)
  
    prettyPrec :: Int -> (Label, Label) -> a -> Doc
    prettyPrec _ _ = pretty

precApp :: Int
precApp = 1

precLam :: Int
precLam = 2

precFun :: Int
precFun = 2

-- Precedence of operators based on the table in:
-- http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
precOp JS.Mult    = 3
precOp JS.Div     = 3
precOp JS.Rem     = 3
precOp JS.Add     = 4
precOp JS.Sub     = 4
precOp JS.LThan   = 6
precOp JS.GThan   = 6
precOp JS.LThanE  = 6
precOp JS.GThanE  = 6
precOp JS.Equal   = 7
precOp JS.NotEq   = 7
precOp JS.CAnd    = 11
precOp JS.COr     = 12
precOp op         = error $ "Something impossible happens! The operator '" 
                            ++ JP.prettyPrint op ++ "' is not part of the language."

name :: Int -> String
name n
    | n < 0     = error "name called with n < 0"
    | n < 26    = [chr (ord 'a' + n)]
    | otherwise = "a" ++ show (n - 26)

instance Pretty (PFTyp Int) where
    prettyPrec p (ltvar, lvar) t = case t of
        FTVar a    -> text (name a)
        FForall f  -> text ("forall " ++ name ltvar ++ ".") <+> prettyPrec p (ltvar+1,  lvar) (f ltvar)
        FFun t1 t2 -> parenPrec p precFun $ 
                        prettyPrec (precFun-1) (ltvar, lvar) t1 
                        <+> text "->" 
                        <+> prettyPrec p (ltvar, lvar) t2
        PFInt      -> text "Int"

instance Pretty (PFExp Int Int) where
    prettyPrec p (ltvar, lvar) e = case e of
        FVar x           -> text (name x)

        FBLam f          -> text ("/\\" ++ name ltvar ++ ".") <+> prettyPrec p (ltvar+1, lvar) (f ltvar)

        FLam t f         -> parenPrec p precLam $ 
                                text ("\\(" ++ name lvar ++ " : " ++ show (prettyPrec p (ltvar, lvar+1) t) ++ ").")
                                <+> prettyPrec p (ltvar, lvar+1) (f lvar)

        FApp e1 e2       -> prettyPrec precApp (ltvar, lvar) e1 <+> prettyPrec precApp (ltvar, lvar) e2 

        FTApp e t        -> prettyPrec p (ltvar, lvar) e <+> prettyPrec p (ltvar, lvar) t

        FPrimOp e1 op e2 -> parenPrec p p' $ 
                                    prettyPrec p' (ltvar, lvar) e1 
                                <+> text (JP.prettyPrint op) 
                                <+> prettyPrec p' (ltvar, lvar) e2 
                                where p' = precOp op 

        FLit n           -> integer n

        Fif0 e1 e2 e3    -> text "if0"      <+> prettyPrec p (ltvar, lvar) e1
                            <+> text "then" <+> prettyPrec p (ltvar, lvar) e2
                            <+> text "else" <+> prettyPrec p (ltvar, lvar) e3

        FTuple es        -> parens $ hcat $ intersperse comma (map (prettyPrec p (ltvar, lvar)) es)

        FProj idx e      -> prettyPrec p (ltvar, lvar) e <> text ("._" ++ show idx)

        FFix t1 f t2     -> text ("fix " ++ name lvar ++ ".")
                            <+> text ("\\(" ++ (name (lvar+1) ++ " : " ++ show (prettyPrec p (ltvar, lvar+2) t1)) ++ ").")
                            <+> prettyPrec p (ltvar, lvar+2) (f lvar (lvar+1)) <+> colon <+> prettyPrec p (ltvar, lvar+2) t2
