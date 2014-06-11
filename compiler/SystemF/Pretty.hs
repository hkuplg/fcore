{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
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

data L = L
    { lx :: Int  -- Fresh label for variables
    , la :: Int  -- Fresh label for type variables
    } deriving (Eq, Show)

class Pretty a where
    pretty :: a -> Doc
    pretty = prettyPrec 0 L{ lx = 1, la = 1 }
  
    prettyPrec :: Int -> L -> a -> Doc
    prettyPrec _ _ = pretty

precFFun :: Int
precFFun = 2

instance Pretty (PFTyp Int) where
    prettyPrec p L{..} (FTVar a)    = text ("A" ++ show a)
    prettyPrec p L{..} (FForall f)  = text ("forall A" ++ show la ++ ".") <+> prettyPrec p L{ la = la + 1, .. } (f la)
    prettyPrec p L{..} (FFun t1 t2) = parenPrec p precFFun $ prettyPrec (precFFun - 1) L{..} t1 <+> text "->" <+> prettyPrec p L{..} t2
    prettyPrec p L{..} PFInt        = text "Int"

instance Pretty (PFExp Int Int) where
    prettyPrec p L{..} (FVar x)           = text ("x" ++ show x)
    prettyPrec p L{..} (FBLam f)          = text "/\\" <> text ("A" ++ show la) <> char '.' <+> prettyPrec p L{ la = la + 1, .. } (f la)
    prettyPrec p L{..} (FLam t f)         = char '\\' <> 
                                                parens (text ("x" ++ show lx) <+> colon <+> prettyPrec p L{ lx = lx + 1, .. } t) <> 
                                                char '.' <+> 
                                                prettyPrec p L{ lx = lx + 1, .. } (f lx)
    prettyPrec p L{..} (FApp e1 e2)       = prettyPrec p L{..} e1 <+> prettyPrec p L{..} e2 
    prettyPrec p L{..} (FTApp e t)        = prettyPrec p L{..} e <+> prettyPrec p L{..} t
    prettyPrec p L{..} (FPrimOp e1 op e2) = prettyPrec p L{..} e1 <+> text (JP.prettyPrint op) <+> prettyPrec p L{..} e2 
    prettyPrec p L{..} (FLit n)           = integer n
    prettyPrec p L{..} (Fif0 e1 e2 e3)    = hsep [text "if0", prettyPrec p L{..} e1, text "then", prettyPrec p L{..} e2, text "else", prettyPrec p L{..} e3]
    prettyPrec p L{..} (FTuple es)        = parens $ hcat $ intersperse comma (map (prettyPrec p L{..}) es)
    prettyPrec p L{..} (FProj idx e)      = prettyPrec p L{..} e <> text ("._" ++ show idx)
    prettyPrec p L{..} (FFix t1 f t2)     = hsep 
                                            [ text "fix" <+> text ("x" ++ show lx) <> char '.'
                                            , char '\\' <> parens (text ("x" ++ show (lx + 1)) <+> colon <+> prettyPrec p L{ lx = lx + 2, .. } t1) <> char '.'
                                            , prettyPrec p L{ lx = lx + 2, la = la + 1 } (f lx (lx + 1)) 
                                            , colon
                                            , prettyPrec p L{ lx = lx + 2, la = la + 1 } t2
                                            ]
