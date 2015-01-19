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
import qualified SystemFI    as F
import qualified Src         as S

import Mixin
import Panic

import Text.PrettyPrint.ANSI.Leijen

import Data.Maybe    (fromMaybe)
import Control.Monad (zipWithM)
import Unsafe.Coerce (unsafeCoerce)

simplify :: F.Expr t e -> Expr t e
simplify = unsafeCoerce . snd . transExpr 0 0 . unsafeCoerce

transType :: Index -> F.Type Index -> Type Index
transType _ (F.TVar n a)     = TVar n a
transType _ (F.JClass c)     = JClass c
transType i (F.Fun a1 a2)    = Fun (transType i a1) (transType i a2)
transType i (F.Forall n f)   = Forall n (\a -> transType (i + 1) $ F.fsubstTT i (F.tVar a) (f i))
transType i (F.Product ts)   = Product (map (transType i) ts)
transType _  F.Unit          = Unit
transType i (F.And a1 a2)    = Product [transType i a1, transType i a2]
transType i (F.Record (_,t)) = transType i t
transType i (F.Thunk t)      = Fun Unit (transType i t)

-- Subtyping
-- For SystemFI.
subtype' :: Class (Index -> F.Type Index -> F.Type Index -> Bool)
subtype' _    _ (F.TVar _ a)   (F.TVar _ b)   = a == b
subtype' _    _ (F.JClass c)   (F.JClass d)   = c == d
subtype' this i (F.Fun t1 t2)  (F.Fun t3 t4)  = this i t3 t1 && this i t2 t4
subtype' this i (F.Forall _ f) (F.Forall _ g) = this (i+1) (f i) (g i)
subtype' this i (F.Product ss) (F.Product ts)
  | length ss /= length ts                    = False
  | otherwise                                 = uncurry (this i) `all` zip ss ts
subtype' _    _  F.Unit         F.Unit        = True
subtype' this i  t1            (F.And t2 t3)  = this i t1 t2 && this i t1 t3
subtype' this i (F.And t1 t2) t3              = this i t1 t3 || this i t2 t3
subtype' this i (F.Record (l1,t1)) (F.Record (l2,t2))
  | l1 == l2                                  = this i t1 t2
  | otherwise                                 = False
subtype' this i  t1            (F.Thunk t2)   = this i t1 t2
subtype' this i (F.Thunk t1)    t2            = this i t1 t2
subtype' _    _  _              _             = False

subtype :: Index -> F.Type Index -> F.Type Index -> Bool
subtype = new subtype'

type Coercion t e = Expr t e

coerce :: Index -> F.Type Index -> F.Type Index -> Maybe (Coercion Index Index)
coerce i (F.TVar n a) (F.TVar _ b) | a == b    = return (lam (transType i (F.TVar n a)) var)
                                   | otherwise = Nothing
coerce i (F.JClass c) (F.JClass d) | c == d    = return (lam (transType i (F.JClass c)) var)
                                   | otherwise = Nothing
coerce i (F.Fun t1 t2) (F.Fun t3 t4) =
  do c1 <- coerce i t3 t1
     c2 <- coerce i t2 t4
     return (lam (transType i (F.Fun t1 t2))
                 (\f -> lam (transType i t3) ((App c2 .  App (var f) . App c1) . var)))
coerce i (F.Forall n f) (F.Forall _ g) =
  do c <- coerce (i + 1) (f i) (g i)
     return (lam (transType i (F.Forall n f)) (\f' -> bLam ((App c . TApp (var f')) . (TVar ""))))
coerce i (F.Product ss) (F.Product ts)
  | length ss /= length ts = Nothing
  | otherwise =
    do cs <- zipWithM (coerce i) ss ts
       let f x = Tuple (zipWith (\c idx -> App c (Proj idx x))
                                 cs [1..length ss])
       return (lam (transType i (F.Product ss)) (f . var))
coerce i F.Unit F.Unit = return (lam (transType i F.Unit) var)
coerce i t1 (F.And t2 t3) =
  do c1 <- coerce i t1 t2
     c2 <- coerce i t1 t3
     return (lam (transType i t1) (\x -> Tuple [App c1 (var x), App c2 (var x)]))
coerce i (F.And t1 t2) t3 =
  case coerce i t1 t3 of
    Just c  -> Just (lam (transType i (F.And t1 t2)) (App c . Proj 1 . var))
    Nothing ->
      case coerce i t2 t3 of
        Nothing -> Nothing
        Just c  -> return (lam (transType i (F.And t1 t2)) (App c . Proj 2 . var))
coerce i (F.Record (l1,t1)) (F.Record (l2,t2)) | l1 == l2  = coerce i t1 t2
                                               | otherwise = Nothing
coerce _ _ _ = Nothing

infer':: Class (Index -> Index -> F.Expr Index (Index, F.Type Index) -> F.Type Index)
infer' _    _ _ (F.Var _ (_,t))      = t
infer' _    _ _ (F.Lit (S.Int _))     = F.JClass "java.lang.Integer"
infer' _    _ _ (F.Lit (S.String _))  = F.JClass "java.lang.String"
infer' _    _ _ (F.Lit (S.Bool _))    = F.JClass "java.lang.Boolean"
infer' _    _ _ (F.Lit (S.Char _))    = F.JClass "java.lang.Character"
infer' _    _ _ (F.Lit  S.UnitLit)    = F.Unit
infer' this i j (F.Lam _ t f)         = F.Fun t (this i (j+1) (f (j,t)))
infer' this i j (F.BLam n f)          = F.Forall n (\a -> F.fsubstTT i (F.tVar a) $ this (i+1) j (f i))
infer' _    _ _ (F.Fix _ _ _ t1 t)    = F.Fun t1 t
infer' this i j (F.Let _ b e)         = this i (j+1) (e (j, this i j b))
infer' this i j (F.LetRec _ ts _ e)   = this i (j+n) (e (zip [j..j+n-1] ts)) where n = length ts
infer' this i j (F.App f _)           = t12                where F.Fun _ t12 = this i j f
infer' this i j (F.TApp f t)          = F.joinType ((unsafeCoerce g :: t -> F.Type t) t) where F.Forall _ g  = this i j f
infer' this i j (F.If _ b1 _)         = this i j b1
infer' _    _ _ (F.PrimOp _ op _)     = case op of S.Arith _   -> F.JClass "java.lang.Integer"
                                                   S.Compare _ -> F.JClass "java.lang.Boolean"
                                                   S.Logic _   -> F.JClass "java.lang.Boolean"
infer' this i j (F.Tuple es)          = F.Product (map (this i j) es)
infer' this i j (F.Proj index e)      = ts !! (index-1) where F.Product ts = this i j e
infer' _    _ _ (F.JNew c _)          = F.JClass c
infer' _    _ _ (F.JMethod _ _ _ c)   = F.JClass c
infer' _    _ _ (F.JField _ _ c)      = F.JClass c
infer' this i j (F.Seq es)            = this i j (last es)
infer' this i j (F.Merge e1 e2)       = F.And (this i j e1) (this i j e2)
infer' this i j (F.RecordIntro (l,e)) = F.Record (l, this i j e)
infer' this i j (F.RecordElim e l1)   = t1 where Just (t1,_) = getter i j (this i j e) l1
infer' this i j (F.RecordUpdate e _)  = this i j e
infer' this i j (F.Lazy e)            = F.Thunk (this i j e)

infer :: Index -> Index -> F.Expr Index (Index, F.Type Index) -> F.Type Index
infer = new infer'

instance (F.Type Index, Expr Index Index) <: F.Type Index where
  up = fst

transExpr'
  :: (Index -> Index -> F.Expr Index (Index, F.Type Index) -> F.Type Index)
  -> (Index -> Index -> F.Expr Index (Index, F.Type Index) -> (F.Type Index, Expr Index Index))
  -> Index  -> Index -> F.Expr Index (Index, F.Type Index) -> Expr Index Index
transExpr' _ _    _ _ (F.Var _ (x,_))      = var x
transExpr' _ _    _ _ (F.Lit l)            = Lit l
transExpr' _ this i j (F.Lam n t f)        = Lam n (transType i t) (\x -> fsubstEE j (var x) body')    where (_, body') = this i (j+1) (f (j, t))
transExpr' _ this i j (F.BLam n f)         = BLam n (\a -> fsubstTE i (TVar "" a) body')               where (_, body') = this (i+1) j (f i)
transExpr' _ this i j (F.Fix n1 n2 f t1 t) = Fix n1 n2 (\x x1 -> (fsubstEE j (var x) . fsubstEE (j+1) (var x1)) body') t1' t'
  where
    (_, body') = this i (j+2) (f (j, F.Fun t1 t) (j+1, t1))
    t1'        = transType i t1
    t'         = transType i t
transExpr' super this i j (F.Let n b e) = Let n b' (\x -> fsubstEE j (var x) (snd (this i (j+1) (e (j, super i j b)))))
  where
    (_,b') = this i j b
transExpr' _     this i j (F.LetRec ns ts bs e) = LetRec ns' ts' bs' e'
  where
    ts'           = map (transType i) ts
    ns'           = ns
    bs' fs'       = map (subst fs fs') bs_body'
    e'  fs'       = subst fs fs' e_body'
    (_, bs_body') = unzip (map (transExpr i (j+n)) (bs fs_with_ts))
    (_, e_body')  = this i (j+n) (e fs_with_ts)
    fs            = [j..j+n-1]
    fs_with_ts    = zip fs ts
    n             = length ts
    subst :: [Index] -> [Index] -> Expr Index Index -> Expr Index Index
    subst xs rs   = foldl (.) id [fsubstEE x (var (rs !! k)) | (x,k) <- zip xs [0..n-1]]

transExpr' _ this i j (F.App e1 e2)
  = let (F.Fun t11 t12, e1') = this i j e1
        (t2, e2')            = this i j e2
    in
    let panic_doc             = text "Coercion failed" <$>
                                text "Function:" <+> pretty_typing e1 (F.Fun t11 t12) <$>
                                text "Argument:" <+> pretty_typing e2 t2 <$>
                                text "Coercion:" <+> pretty_coercion t2 t11
        pretty_typing e t     = F.prettyExpr (unsafeCoerce e :: F.Expr Index Index) <+> colon <+>
                                F.prettyType (unsafeCoerce t :: F.Type Index)
        pretty_coercion s1 s2 = F.prettyType (unsafeCoerce s1 :: F.Type Index) <+> text "<:" <+> F.prettyType (unsafeCoerce s2 :: F.Type Index)
    in
    case t11 of
     F.Thunk t11_naked ->
       let c = fromMaybe (prettyPanic "Simplify.transExpr'" panic_doc) (coerce i t2 t11_naked)
       in App e1' (wrap (App c e2'))
     _                 ->
        let c = fromMaybe (prettyPanic "Simplify.transExpr'" panic_doc) (coerce i t2 t11)
        in App e1' (App c e2')

transExpr' _ this i j (F.TApp e t)                   = TApp (snd (this i j e)) (transType i t)
transExpr' _ this i j (F.If p b1 b2)                 = If (snd (this i j p)) (snd (this i j b1)) (snd (this i j b2))
transExpr' _ this i j (F.PrimOp e1 op e2)            = PrimOp (snd (this i j e1)) op (snd (this i j e2))
transExpr' _ this i j (F.Tuple es)                   = Tuple (snd (unzip (map (this i j) es)))
transExpr' _ this i j (F.Proj index e)               = Proj index (snd (this i j e))
transExpr' _ this i j (F.JNew c es)                  = JNew c (snd (unzip (map (this i j) es)))

-- At the moment, in `System.out.println(x)`, `x` can be left as a thunk even
-- after the simplification. We need to recursively force the arguments of a
-- Java method call. The current solution is not very neat. It'd be better to
-- uniformly address all similar concerns for `App`, `JMethod`, and `PrimOp`.
transExpr' _ this i j (F.JMethod callee m args ret)
  = let args' = map (forceLazy . this i j) args in
    JMethod (fmap (snd . this i j) callee) m args' ret
  where
    forceLazy (F.Thunk _, e) = force e
    forceLazy (_,e)          = e

transExpr' _ this i j (F.JField callee m ret)        = JField (fmap (snd . this i j) callee) m ret
transExpr' _ this i j (F.Seq es)                     = Seq (snd (unzip (map (this i j) es)))
transExpr' _ this i j (F.Merge e1 e2)                = Tuple [snd (this i j e1), snd (this i j e2)]
transExpr' _ this i j (F.RecordIntro (_,e))          = snd (this i j e)
transExpr' super this i j (F.RecordElim e l1)        = App c (snd (this i j e)) where Just (_,c) = getter i j (super i j e) l1
transExpr' super this i j (F.RecordUpdate e (l1,e1)) = App c (snd (this i j e)) where Just (_,c) = putter i j (super i j e) l1 (snd (this i j e1))
transExpr' _ this i j (F.Lazy e)                     = lam Unit (\_ -> snd (this i j e))

transExpr :: Index -> Index -> F.Expr Index (Index, F.Type Index) -> (F.Type Index, Expr Index Index)
transExpr = new (infer' `with` transExpr'')
  where
    transExpr'' :: Mixin
      (Index -> Index -> F.Expr Index (Index, F.Type Index) -> F.Type Index)
      (Index -> Index -> F.Expr Index (Index, F.Type Index) -> (F.Type Index, Expr Index Index))
    transExpr'' super this i j e  = (super i j e, transExpr' super this i j e)

getter :: Index -> Index -> F.Type Index -> S.Label -> Maybe (F.Type Index, Expr Index Index)
getter i j (F.Record (l,t)) l1
  | l1 == l   = Just (t, lam (transType i (F.Record (l,t))) var)
  | otherwise = Nothing
getter i j (F.And t1 t2) l
  = case getter i j t2 l of
      Just (t,c) ->
        Just (t, lam (transType i (F.And t1 t2)) (App c . Proj 2 . var))
      Nothing    ->
        case getter i j t1 l of
          Nothing    -> Nothing
          Just (t,c) ->
            Just (t, lam (transType i (F.And t1 t2)) (App c . Proj 1 . var))
getter i j (F.Thunk t1) l
  = do (t,c) <- getter i j t1 l
       return (t, lam (transType i (F.Thunk t1)) (App c . force . var))
getter _ _ _ _ = Nothing

putter :: Index -> Index -> F.Type Index -> S.Label -> Expr Index Index -> Maybe (F.Type Index, Expr Index Index)
putter i j (F.Record (l,t)) l1 e
  | l1 == l   = Just (t, Simplify.const (transType i (F.Record (l,t))) (unsafeCoerce . snd . transExpr i j . unsafeCoerce $ e))
  | otherwise = Nothing
putter i j (F.And t1 t2) l e
  = case putter i j t2 l e of
      Just (t,c) ->
        Just (t, lam (transType i (F.And t1 t2)) (\x -> Tuple [Proj 1 (var x), App c (Proj 2 (var x))]))
      Nothing    ->
        case putter i j t1 l e of
          Nothing    -> Nothing
          Just (t,c) ->
            Just (t, lam (transType i (F.And t1 t2)) (\x -> Tuple [App c (Proj 1 (var x)), Proj 2 (var x)]))
putter i j (F.Thunk t1) l e
  = do (t,c) <- putter i j t1 l e
       return (t, lam (transType i (F.Thunk t1)) (App c . force . var))
putter _ _ _ _ _ = Nothing

wrap :: Expr t e -> Expr t e
wrap e = lam Unit (Prelude.const e)

force :: Expr t e -> Expr t e
force e = App e (Lit S.UnitLit)

const :: Type t -> Expr t e -> Expr t e
const t e = lam t (Prelude.const e)
