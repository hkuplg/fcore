module Phoas where

import PrettyUtils
import Text.PrettyPrint.Leijen

data Type t = TVar t | Int | Fun (Type t) (Type t) | Forall (t -> Type t)

prettyType :: PrecedenceEnv -> Int -> Type Int -> Doc
prettyType p i (TVar a)      = text (nameTVar a)
prettyType p i Int           = text "Int"
prettyType p i (Forall f)    = parensIf p 1 (text "forall" <+> text (nameTVar i) <> dot <+> prettyType (1,PrecMinus) (succ i)  (f i))
prettyType p i (t1 `Fun` t2) = parensIf p 2 (prettyType (2,PrecPlus) i t1 <+> text "->" <+> prettyType (2,PrecMinus) i t2)

joinType :: Type (Type t) -> Type t
joinType (TVar a)    = a
joinType Int         = Int
joinType (Fun t1 t2) = Fun (joinType t1) (joinType t2)
joinType (Forall f)  = Forall (\a -> joinType (f (TVar a)))

konstTyp = Forall (\a -> Forall (\b -> TVar a `Fun` (TVar b `Fun` TVar a)))

tapp = let Forall f = konstTyp
       in joinType (f Int)

r1 :: Doc
r1 = prettyType basePrecEnv 0 tapp

r2 :: Doc
r2 = prettyType basePrecEnv 1 tapp
