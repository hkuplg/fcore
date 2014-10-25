-- The simplifier: translate System F with intersection types to vanilla System F

{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Simplify
  ( simplify
  , transType
  , subtype'
  , subtype
  , coerce
  , infer'
  , infer
  , transExpr'
  , transExpr
  , getter
  , putter
  ) where

import Core
import qualified Src as S

import Mixin

import Data.Maybe    (fromMaybe)
import Control.Monad (zipWithM)
import Unsafe.Coerce (unsafeCoerce)

simplify :: Expr t e -> Expr t e
simplify = unsafeCoerce . snd . transExpr 0 0 . unsafeCoerce

transType :: Index -> Type Index -> Type Index
transType _ (TVar a)         = TVar a
transType _ (JClass c)       = JClass c
transType i (Fun a1 a2)      = Fun (transType i a1) (transType i a2)
transType i (Forall f)       = Forall (\a -> transType (i + 1) $ fsubstTT i (TVar a) (f i))
transType i (Product ts)     = Product (map (transType i) ts)
transType i (And a1 a2)      = Product [transType i a1, transType i a2]
transType i (RecordTy (_,t)) = transType i t

-- Subtyping

-- A `Coercion` is either an identity function or some non-trivial function.
-- The purpose is to avoid applying the identity functions to an expression.
data Coercion t e = Id | C (Expr t e)

isIdC :: Coercion t e -> Bool
isIdC Id    = True
isIdC (C _) = False

appC :: Coercion t e -> Expr t e -> Expr t e
appC Id e      = e
appC (C e1) e2 = App e1 e2

subtype' :: Class (Index -> Type Index -> Type Index -> Bool)
subtype' _    _ (TVar a)    (TVar b)   = a == b
subtype' _    _ (JClass c)   (JClass d)  = c == d
subtype' this i (Fun t1 t2)  (Fun t3 t4) = this i t3 t1 && this i t2 t4
subtype' this i (Forall f)   (Forall g)  = this (i+1) (f i) (g i)
subtype' this i (Product ss) (Product ts)
  | length ss /= length ts               = False
  | otherwise                            = uncurry (this i) `all` zip ss ts
subtype' this i t1 (And t2 t3)           = this i t1 t2 || this i t1 t3
subtype' this i (And t1 t2) t3           = this i t1 t3 && this i t2 t3
subtype' this i (RecordTy (l1,t1)) (RecordTy (l2,t2))
  | l1 == l2                             = this i t1 t2
  | otherwise                            = False
subtype' _    _ _ _                      = False

subtype :: Index -> Type Index -> Type Index -> Bool
subtype = new subtype'

coerce :: Index -> Type Index -> Type Index -> Maybe (Coercion Index Index)
coerce _ (TVar a) (TVar b) | a == b        = return Id
                           | otherwise     = Nothing
coerce _ (JClass c) (JClass d) | c == d    = return Id
                               | otherwise = Nothing
coerce i (Fun t1 t2) (Fun t3 t4) =
  do c1 <- coerce i t3 t1
     c2 <- coerce i t2 t4
     case (c1,c2) of
       (Id,Id) -> return Id
       (_,_)   ->
         return (C (Lam (transType i (Fun t1 t2))
                      (\f -> Lam (transType i t3)
                               ((appC c2 .  App (Var f) . appC c1) . Var))))
coerce i (Forall f) (Forall g) =
  do c <- coerce (i + 1) (f i) (g i)
     case c of
       Id -> return Id
       _  -> return (C (Lam (transType i (Forall f))
                         (\f' -> BLam ((appC c . TApp (Var f')) . TVar))))
coerce i (Product ss) (Product ts)
  | length ss /= length ts = Nothing
  | otherwise =
    do cs <- zipWithM (coerce i) ss ts
       if isIdC `all` cs
          then return Id
          else
            let f x = Tuple (zipWith (\c idx -> appC c (Proj idx x))
                                     cs [1.. (length ss)])
            in
            return (C (Lam (transType i (Product ss)) (f . Var)))
coerce i t1 (And t2 t3) =
  do c1 <- coerce i t1 t2
     c2 <- coerce i t1 t3
     case (c1,c2) of
       (Id,Id) -> return Id
       (_,_)   -> return (C (Lam (transType i t1)
                              (\x -> Tuple [c1 `appC` Var x, c2 `appC` Var x])))
coerce i (And t1 t2) t3 =
  case coerce i t1 t3 of
    Just c  -> Just (C (Lam (transType i (And t1 t2)) (\x -> c `appC` Proj 1 (Var x))))
    Nothing ->
      case coerce i t2 t3 of
        Nothing -> Nothing
        Just c  -> return (C (Lam (transType i (And t1 t2))
                               (\x -> c `appC` Proj 2 (Var x))))
coerce i (RecordTy (l1,t1)) (RecordTy (l2,t2)) | l1 == l2  = coerce i t1 t2
                                               | otherwise = Nothing
coerce _ _ _ = Nothing

infer':: Class (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
infer' _    _ _ (Var (_,t))         = t
infer' _    _ _ (Lit (S.Integer _)) = JClass "java.lang.Integer"
infer' _    _ _ (Lit (S.String _))  = JClass "java.lang.String"
infer' _    _ _ (Lit (S.Boolean _)) = JClass "java.lang.Boolean"
infer' _    _ _ (Lit (S.Char _))    = JClass "java.lang.Character"
infer' _    _ _ (Lit  S.Unit)       = JClass "java.lang.Integer"
infer' this i j (Lam t f)           = Fun t (this i (j+1) (f (j,t)))
infer' this i j (BLam f)            = Forall (\a -> fsubstTT i (TVar a) $ this (i+1) j (f i))
infer' _    _ _ (Fix _ t1 t)        = Fun t1 t
infer' this i j (Let b e)           = this i (j+1) (e (j, this i j b))
infer' this i j (LetRec ts _ e)     = this i (j+n) (e (zip [j..j+n-1] ts)) where n = length ts
infer' this i j (App f _)           = t12                where Fun _ t12 = this i j f
infer' this i j (TApp f t)          = joinType ((unsafeCoerce g :: t -> Type t) t) where Forall g  = this i j f
infer' this i j (If _ b1 _)         = this i j b1
infer' _    _ _ (PrimOp _ op _)     = case op of S.Arith _   -> JClass "java.lang.Integer"
                                                 S.Compare _ -> JClass "java.lang.Boolean"
                                                 S.Logic _   -> JClass "java.lang.Boolean"
infer' this i j (Tuple es)          = Product (map (this i j) es)
infer' this i j (Proj index e)      = ts !! (index-1) where Product ts = this i j e
infer' _    _ _ (JNew c _)          = JClass c
infer' _    _ _ (JMethod _ _ _ c)   = JClass c
infer' _    _ _ (JField _ _ c)      = JClass c
infer' this i j (Seq es)            = this i j (last es)
infer' this i j (Merge e1 e2)       = And (this i j e1) (this i j e2)
infer' this i j (RecordIntro (l,e)) = RecordTy (l, this i j e)
infer' this i j (RecordElim e l1)   = t1 where Just (_,t1) = getter i (this i j e) l1
infer' this i j (RecordUpdate e _)  = this i j e

infer :: Index -> Index -> Expr Index (Index, Type Index) -> Type Index
infer = new infer'

instance (Type Index, Expr Index Index) <: Type Index where
  up = fst

transExpr'
  :: (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
  -> (Index -> Index -> Expr Index (Index, Type Index) -> (Type Int, Expr Index Index))
  -> Index  -> Index -> Expr Index (Index, Type Index) -> Expr Index Index
transExpr' _ _    _ _ (Var (x,_))  = Var x
transExpr' _ _    _ _ (Lit l)      = Lit l
transExpr' _ this i j (Lam t f)    = Lam (transType i t) (\x -> fsubstEE j (Var x) body') where (_, body') = this i     (j+1) (f (j, t))
transExpr' _ this i j (BLam f)     = BLam (\a -> fsubstTE i (TVar a) body')               where (_, body') = this (i+1) j     (f i)
transExpr' _ this i j (Fix f t1 t) = Fix (\x x1 -> (fsubstEE j (Var x) . fsubstEE (j+1) (Var x1)) body') t1' t'
  where
    (_, body') = this i (j+2) (f (j, Fun t1 t) (j+1, t1))
    t1'        = transType i t1
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
transExpr' _ this i j (App e1 e2) = App e1' (appC c e2')
  where
    (t1@(Fun t11 _), e1') = this i j e1
    (t2, e2')        = this i j e2
    c                = fromMaybe (error $ "\n" ++ show (unsafeCoerce e1 :: Expr Index Index) ++ " :: " ++ show t1  ++ "\n"
                                            ++ show (unsafeCoerce e2 :: Expr Index Index) ++ " :: " ++ show t2) $ coerce i t2 t11
transExpr' _ this i j (TApp e t)                   = TApp (snd (this i j e)) (transType i t)
transExpr' _ this i j (If p b1 b2)                 = If (snd (this i j p)) (snd (this i j b1)) (snd (this i j b2))
transExpr' _ this i j (PrimOp e1 op e2)            = PrimOp (snd (this i j e1)) op (snd (this i j e2))
transExpr' _ this i j (Tuple es)                   = Tuple (snd (unzip (map (this i j) es)))
transExpr' _ this i j (Proj index e)               = Proj index (snd (this i j e))
transExpr' _ this i j (JNew c es)                  = JNew c (snd (unzip (map (this i j) es)))
transExpr' _ this i j (JMethod callee m args ret)  = JMethod (fmap (snd . this i j) callee) m (snd (unzip (map (this i j) args))) ret
transExpr' _ this i j (JField callee m ret)        = JField (fmap (snd . this i j) callee) m ret
transExpr' _ this i j (Seq es)                     = Seq (snd (unzip (map (this i j) es)))
transExpr' _ this i j (Merge e1 e2)                = Tuple [snd (this i j e1), snd (this i j e2)]
transExpr' _ this i j (RecordIntro (_,e))          = snd (this i j e)
transExpr' super this i j (RecordElim e l1)        = appC c (snd (this i j e)) where Just (c, _) = getter i (super i j e) l1
transExpr' super this i j (RecordUpdate e (l1,e1)) = appC c (snd (this i j e)) where Just (c, _) = putter i (super i j e) l1 (snd (this i j e1))

transExpr :: Index -> Index -> Expr Index (Index, Type Index) -> (Type Index, Expr Index Index)
transExpr = new (infer' `with` transExpr'')
  where
    transExpr'' :: Mixin
      (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
      (Index -> Index -> Expr Index (Index, Type Index) -> (Type Index, Expr Index Index))
    transExpr'' super this i j e  = (super i j e, transExpr' super this i j e)

getter :: Index -> Type Index -> S.Label -> Maybe (Coercion Index Index, Type Index)
getter _ (RecordTy (l,t)) l1
  | l1 == l   = Just (Id, t)
  | otherwise = Nothing
getter i (And t1 t2) l
  = case getter i t2 l of
      Just (c,t) ->
        Just (C $ Lam (transType i (And t1 t2)) (appC c . Proj 2 . Var)
             ,t)
      Nothing    ->
        case getter i t1 l of
          Nothing    -> Nothing
          Just (c,t) ->
            Just (C $ Lam (transType i (And t1 t2)) (appC c . Proj 1 . Var)
                 ,t)
getter _ _ _ = Nothing

putter :: Index -> Type Index -> S.Label -> Expr Index Index -> Maybe (Coercion Index Index, Type Index)
putter i (RecordTy (l,t)) l1 e
  | l1 == l   = Just (C $ coreConst (transType i $ RecordTy (l,t)) e, t)
  | otherwise = Nothing
putter i (And t1 t2) l e
  = case putter i t2 l e of
      Just (c,t) ->
        case c of
          Id   -> Just (Id,t)
          C _  -> Just (C $ Lam (transType i (And t1 t2))
                              (\x -> Tuple [Proj 1 (Var x), appC c (Proj 2 (Var x))])
                       ,t)
      Nothing    ->
        case putter i t1 l e of
          Nothing    -> Nothing
          Just (c,t) ->
            case c of
              Id   -> Just (Id, t)
              C _  -> Just (C $ Lam (transType i (And t1 t2))
                                  (\x -> Tuple [appC c (Proj 1 (Var x)), Proj 2 (Var x)])
                           ,t)
putter _ _ _ _ = Nothing

-- Core's id, specialized to type t.
coreId :: Type Index -> Expr Index Index
coreId t = Lam t Var

-- Core's const, specialized to type t.
coreConst :: Type Index -> Expr Index Index -> Expr Index Index
coreConst t e = Lam t (const e)
