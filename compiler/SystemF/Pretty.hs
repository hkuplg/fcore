{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module SystemF.Pretty where

import Text.PrettyPrint
import qualified Language.Java.Pretty as JP
import Data.List        (intercalate)

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
    prettyPrec p i (FFun t1 t2) = parenPrec p precFFun $ hsep $ [prettyPrec (precFFun - 1) i t1, text "->", prettyPrec p i t2]
    prettyPrec p i PFInt        = text "Int"

-- prettyPFExp :: PFExp Int Int -> Int -> String     
-- prettyPFExp (FVar n)   _         = "x" ++ show n
-- prettyPFExp (FBLam f)  i         = "(" ++ "/\\A" ++ show i ++ ". " ++ prettyPFExp (f i) (i+1) ++ ")" 
-- prettyPFExp (FLam t f) i         = "(" ++ "\\(x" ++ show i ++ " : " ++ prettyPFTyp t (i+1) False ++ "). " ++ prettyPFExp (f i) (i+1) ++ ")" 
-- prettyPFExp (FApp e1 e2) i       = "(" ++ prettyPFExp e1 i ++ ") (" ++ prettyPFExp e2 i ++ ")"
-- prettyPFExp (FTApp e t) i        = "(" ++ prettyPFExp e i ++ ") (" ++ prettyPFTyp t i False ++ ")"
-- prettyPFExp (FPrimOp e1 op e2) i = "(" ++ prettyPFExp e1 i ++ " " ++ JP.prettyPrint op ++ " " ++ prettyPFExp e2 i ++ ")"
-- prettyPFExp (FLit n) _           = show n
-- prettyPFExp (Fif0 e1 e2 e3) i    = "(if " ++ prettyPFExp e1 i ++ " then " ++ prettyPFExp e2 i ++ " else " ++ prettyPFExp e3 i ++ ")"
-- prettyPFExp (FTuple es) i        = "(" ++ intercalate "," (map (\e -> prettyPFExp e i) es)
-- prettyPFExp (FProj pos e) i      = "(" ++ prettyPFExp e i ++ "._" ++ show pos ++ ")"
-- prettyPFExp (FFix t1 f t2) i     = "(fix x" ++ show i ++ ". \\(x" ++ show (i+1) ++ " : " ++ prettyPFTyp t1 i False ++ "). " ++ 
--                                     prettyPFExp (f i (i+1)) (i+2) ++ " : " ++ prettyPFTyp t2 i False ++ ")"
