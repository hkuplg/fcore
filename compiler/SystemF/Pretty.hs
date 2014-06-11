{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module SystemF.Pretty where

import qualified Language.Java.Syntax as JS
import qualified Language.Java.Pretty as JP
import Text.PrettyPrint
import Data.List        (intersperse)

import SystemF.Syntax


prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
    | inheritedPrec <= 0          = t
    | inheritedPrec < currentPrec = parens t
    | otherwise                   = t

class Pretty a where
    pretty :: a -> Doc
    pretty = prettyPrec 0 1 
  
    prettyPrec :: Int -> Int -> a -> Doc
    prettyPrec _ _ = pretty

precFFun :: Int
precFFun = 2

instance Pretty (PFTyp Int) where
    prettyPrec p i (FTVar a)    = text ("A" ++ show a)
    prettyPrec p i (FForall f)  = text ("forall A" ++ show i ++ ".") <+> prettyPrec p (i+1) (f i)
    prettyPrec p i (FFun t1 t2) = parenPrec p precFFun $ prettyPrec (precFFun - 1) i t1 <+> text "->" <+> prettyPrec p i t2
    prettyPrec p i PFInt        = text "Int"

instance Pretty (PFExp Int Int) where
    prettyPrec p i (FVar x)           = text ("x" ++ show x)
    prettyPrec p i (FBLam f)          = text ("/\\A" ++ show i ++ ".") <+> prettyPrec p (i+1) (f i)
    prettyPrec p i (FLam t f)         = char '\\' <> parens (text ("x" ++ show i) <+> colon <+> prettyPrec p (i+1) t) <> char '.' <+> prettyPrec p (i+1) (f i)
    prettyPrec p i (FApp e1 e2)       = prettyPrec p i e1 <+> prettyPrec p i e2 
    prettyPrec p i (FTApp e t)        = prettyPrec p i e <+> prettyPrec p i t
    prettyPrec p i (FPrimOp e1 op e2) = prettyPrec p i e1 <+> text (JP.prettyPrint op) <+> prettyPrec p i e2 
    prettyPrec p i (FLit n)           = integer n
    prettyPrec p i (Fif0 e1 e2 e3)    = hsep [text "if", prettyPrec p i e1, text "then", prettyPrec p i e2, text "else", prettyPrec p i e3]
    prettyPrec p i (FTuple es)        = parens $ hcat $ intersperse comma (map (prettyPrec p i) es)
    prettyPrec p i (FProj idx e)      = prettyPrec p i e <> text ("._" ++ show idx)
    prettyPrec p i (FFix t1 f t2)     = hsep [ text ("fix x" ++ show i ++ ".")
                                             , char '\\' <> parens (text ("x" ++ show (i+1)) <+> colon <+> prettyPrec p (i+2) t1) <> char '.'
                                             , prettyPrec p (i+2) (f i (i+1)) 
                                             , colon
                                             , prettyPrec p (i+2) t2
                                             ]
