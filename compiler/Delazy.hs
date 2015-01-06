-- Elaborating System F with mixed evaluation strategies (call-by-name /
-- call-by-value) to that with call-by-value strategy.

{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Delazy
  ( EvaluationStrategy(..)
  , delazy
  ) where

import Core
import qualified Src as S

import Mixin

import Control.Monad (zipWithM)
import Unsafe.Coerce (unsafeCoerce)

data EvaluationStrategy = CallByValue | CallByName

delazy :: EvaluationStrategy -> Expr t e -> Expr t e
delazy _ = id

transTypeS :: Index -> TypeS Index -> TypeS Index
transTypeS i (Strict t) = Strict (transType i t)
transTypeS i (Lazy   t) = Strict (Fun (Strict UnitType) (transType i t))

transType :: Index -> Type Index -> Type Index
transType _ (TVar a)         = TVar a
transType _ (JClass c)       = JClass c
transType i (Fun t1 t2)      = Fun (transTypeS i t1) (transType i t2)
transType i (Forall f)       = Forall (\a -> transType (i + 1) $ fsubstTT i (TVar a) (f i))
transType i (Product ts)     = Product (map (transType i) ts)
transType _  UnitType        = UnitType

infer':: Class ((Type Index -> TypeS Index) -> Index -> Index -> Expr Index (Index, TypeS Index) -> TypeS Index)
infer' _    s _ _ (Var (_,t))         = t
infer' _    s _ _ (Lit (S.Integer _)) = s (JClass "java.lang.Integer")
infer' _    s _ _ (Lit (S.String _))  = s (JClass "java.lang.String")
infer' _    s _ _ (Lit (S.Boolean _)) = s (JClass "java.lang.Boolean")
infer' _    s _ _ (Lit (S.Char _))    = s (JClass "java.lang.Character")
infer' _    s _ _ (Lit  S.Unit)       = s UnitType
infer' this s i j (Lam t f)           = s $ Fun t (this s i (j+1) (f (j,t)))
infer' this s i j (BLam f)            = Forall (\a -> fsubstTT i (TVar a) $  $ this s (i+1) j (f i))
infer' _    s _ _ (Fix _ _ _ t1 t)        = Fun t1 t
infer' this s i j (Let b e)           = this s i (j+1) (e (j, this s i j b))
infer' this s i j (LetRec ts _ e)     = this s i (j+n) (e (zip [j..j+n-1] (map s ts))) where n = length ts
infer' this s i j (App f _)           = s $ t12 where Fun _ t12 = this s i j f
infer' this s i j (TApp f t)          = s $ joinType ((unsafeCoerce g :: t -> Type t) t) where Forall g  = this i j f
infer' this s i j (If _ b1 _)         = this s i j b1
infer' _    s _ _ (PrimOp _ op _)     = s $ case op of S.Arith _   -> JClass "java.lang.Integer"
                                                       S.Compare _ -> JClass "java.lang.Boolean"
                                                       S.Logic _   -> JClass "java.lang.Boolean"
infer' this s i j (Tuple es)          = s $ Product (map (this s i j) es)
infer' this s i j (Proj index e)      = s $ ts !! (index-1) where Product ts = this s i j e
infer' _    s _ _ (JNew c _)          = s $ JClass c
infer' _    s _ _ (JMethod _ _ _ c)   = s $ JClass c
infer' _    s _ _ (JField _ _ c)      = s $ JClass c
infer' this s i j (Seq es)            = this i j (last es)

infer :: Index -> Index -> Expr Index (Index, TypeS Index) -> TypeS Index
infer = new infer'

instance (Type Index, Expr Index Index) <: Type Index where
  up = fst

transExpr'
  :: (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
  -> (Index -> Index -> Expr Index (Index, Type Index) -> (Type Int, Expr Index Index))
  -> Index  -> Index -> Expr Index (Index, Type Index) -> Expr Index Index
transExpr' _ _    _ _ (Var (x,_))       = Var x
transExpr' _ _    _ _ (Lit l)           = Lit l
transExpr' _ this i j (Lam t f)         = Lam (transTypeS i t) (\x -> fsubstEE j (Var x) body') where (_, body') = this i (j+1) (f (j, unS t))
transExpr' _ this i j (BLam f)          = BLam (\a -> fsubstTE i (TVar a) body')               where (_, body') = this (i+1) j (f i)
transExpr' _ this i j (Fix n1 n2 f t1 t)      = Fix n1 n2 (\x x1 -> (fsubstEE j (Var x) . fsubstEE (j+1) (Var x1)) body') t1' t'
  where
    (_, body') = this i (j+2) (f (j, Fun t1 t) (j+1, unS t1))
    t1'        = transTypeS i t1
    t'         = transType i t
transExpr' super this i j (Let b e) = Let b' (\x -> fsubstEE j (Var x) (snd (this i (j+1) (e (j, super i j b)))))
  where
    (_,b') = this i j b
transExpr' _     this i j (LetRec ts bs e) = LetRec ts' bs' e'
  where
    ts'           = map (transType i) ts
    bs'           = \fs' -> map (subst fs fs') bs_body'
    e'            = \fs' -> subst fs fs' e_body'
    (_, bs_body') = unzip (map (transExpr i (j+n)) (bs fs_with_ts))
    (_, e_body')  = this i (j+n) (e fs_with_ts)
    fs            = [j..j+n-1]
    fs_with_ts    = zip fs ts
    n             = length ts
    subst :: [Index] -> [Index] -> Expr Index Index -> Expr Index Index
    subst xs rs   = foldl (.) id [fsubstEE x (Var (rs !! k)) | (x,k) <- zip xs [0..n-1]]
transExpr' _ this i j (App e1 e2) = App e1 e2
transExpr' _ this i j (TApp e t)                   = TApp (snd (this i j e)) (transType i t)
transExpr' _ this i j (If p b1 b2)                 = If (snd (this i j p)) (snd (this i j b1)) (snd (this i j b2))
transExpr' _ this i j (PrimOp e1 op e2)            = PrimOp (snd (this i j e1)) op (snd (this i j e2))
transExpr' _ this i j (Tuple es)                   = Tuple (snd (unzip (map (this i j) es)))
transExpr' _ this i j (Proj index e)               = Proj index (snd (this i j e))
transExpr' _ this i j (JNew c es)                  = JNew c (snd (unzip (map (this i j) es)))
transExpr' _ this i j (JMethod callee m args ret)  = JMethod (fmap (snd . this i j) callee) m (snd (unzip (map (this i j) args))) ret
transExpr' _ this i j (JField callee m ret)        = JField (fmap (snd . this i j) callee) m ret
transExpr' _ this i j (Seq es)                     = Seq (snd (unzip (map (this i j) es)))

transExpr :: Index -> Index -> Expr Index (Index, Type Index) -> (Type Index, Expr Index Index)
transExpr = new (infer' `with` transExpr'')
  where
    transExpr'' :: Mixin
      (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
      (Index -> Index -> Expr Index (Index, Type Index) -> (Type Index, Expr Index Index))
    transExpr'' super this i j e  = (super i j e, transExpr' super this i j e)
