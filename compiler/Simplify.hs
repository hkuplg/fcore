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
import qualified SystemFI    as FI
import qualified Src         as S

import Mixin
import Panic

import Text.PrettyPrint.ANSI.Leijen

import Data.Maybe    (fromMaybe)
import Control.Monad (zipWithM)
import Unsafe.Coerce (unsafeCoerce)

simplify :: FI.Expr t e -> Expr t e
simplify = unsafeCoerce . snd . transExpr 0 0 . unsafeCoerce

transType :: Index -> FI.Type Index -> Type Index
transType _ (FI.TVar n a)     = TVar n a
transType _ (FI.JClass c)     = JClass c
transType i (FI.Fun a1 a2)    = Fun (transType i a1) (transType i a2)
transType i (FI.Forall n f)   = Forall n (\a -> transType (i + 1) $ FI.fsubstTT i (FI.TVar n a) (f i))
transType i (FI.Product ts)   = Product (map (transType i) ts)
transType _  FI.Unit          = Unit
transType i (FI.And a1 a2)    = Product [transType i a1, transType i a2]
transType i (FI.Record (_,t)) = transType i t
transType _ (FI.Datatype n ns) = Datatype n ns

-- Subtyping
-- For SystemFI.
subtype' :: Class (Index -> FI.Type Index -> FI.Type Index -> Bool)
subtype' _    _ (FI.TVar _ a)   (FI.TVar _ b)   = a == b
subtype' _    _ (FI.JClass c)   (FI.JClass d)   = c == d
subtype' this i (FI.Fun t1 t2)  (FI.Fun t3 t4)  = this i t3 t1 && this i t2 t4
subtype' this i (FI.Forall _ f) (FI.Forall _ g) = this (i+1) (f i) (g i)
subtype' this i (FI.Product ss) (FI.Product ts)
  | length ss /= length ts                    = False
  | otherwise                                 = uncurry (this i) `all` zip ss ts
subtype' _    _  FI.Unit         FI.Unit        = True
subtype' this i  t1            (FI.And t2 t3)  = this i t1 t2 && this i t1 t3
subtype' this i (FI.And t1 t2) t3              = this i t1 t3 || this i t2 t3
subtype' this i (FI.Record (l1,t1)) (FI.Record (l2,t2))
  | l1 == l2                                  = this i t1 t2
  | otherwise                                 = False
subtype' _ _ (FI.Datatype n1 _) (FI.Datatype n2 _)  = n1 == n2 -- TODO
subtype' _    _  _              _             = False

subtype :: Index -> FI.Type Index -> FI.Type Index -> Bool
subtype = new subtype'

type Coercion t e = Expr t e

coerce :: Index -> FI.Type Index -> FI.Type Index -> Maybe (Coercion Index Index)
coerce i (FI.TVar n a) (FI.TVar _ b) | a == b    = return (lam (transType i (FI.TVar n a)) var)
                                   | otherwise = Nothing
coerce i (FI.JClass c) (FI.JClass d) | c == d    = return (lam (transType i (FI.JClass c)) var)
                                   | otherwise = Nothing
coerce i (FI.Fun t1 t2) (FI.Fun t3 t4) =
  do c1 <- coerce i t3 t1
     c2 <- coerce i t2 t4
     return (lam (transType i (FI.Fun t1 t2))
                 (\f -> lam (transType i t3) ((App c2 .  App (var f) . App c1) . var)))
coerce i (FI.Forall n f) (FI.Forall _ g) =
  do c <- coerce (i + 1) (f i) (g i)
     return (lam (transType i (FI.Forall n f)) (\f' -> bLam ((App c . TApp (var f')) . TVar "")))
coerce i (FI.Product ss) (FI.Product ts)
  | length ss /= length ts = Nothing
  | otherwise =
    do cs <- zipWithM (coerce i) ss ts
       let f x = Tuple (zipWith (\c idx -> App c (Proj idx x))
                                 cs [1..length ss])
       return (lam (transType i (FI.Product ss)) (f . var))
coerce i FI.Unit FI.Unit = return (lam (transType i FI.Unit) var)
coerce i t1 (FI.And t2 t3) =
  do c1 <- coerce i t1 t2
     c2 <- coerce i t1 t3
     return (lam (transType i t1) (\x -> Tuple [App c1 (var x), App c2 (var x)]))
coerce i (FI.And t1 t2) t3 =
  case coerce i t1 t3 of
    Just c  -> Just (lam (transType i (FI.And t1 t2)) (App c . Proj 1 . var))
    Nothing ->
      case coerce i t2 t3 of
        Nothing -> Nothing
        Just c  -> return (lam (transType i (FI.And t1 t2)) (App c . Proj 2 . var))
coerce i (FI.Record (l1,t1)) (FI.Record (l2,t2)) | l1 == l2  = coerce i t1 t2
                                               | otherwise = Nothing
coerce i d@(FI.Datatype n1 _) (FI.Datatype n2 _) | n1 == n2  = return (lam (transType i d) var)
                                               | otherwise = Nothing
coerce _ _ _ = Nothing

infer' :: Class (Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> FI.Type Index)
infer' _    _ _ (FI.Var _ (_,t))      = t
infer' _    _ _ (FI.Lit (S.Int _))     = FI.JClass "java.lang.Integer"
infer' _    _ _ (FI.Lit (S.String _))  = FI.JClass "java.lang.String"
infer' _    _ _ (FI.Lit (S.Bool _))    = FI.JClass "java.lang.Boolean"
infer' _    _ _ (FI.Lit (S.Char _))    = FI.JClass "java.lang.Character"
infer' _    _ _ (FI.Lit  S.UnitLit)    = FI.Unit
infer' this i j (FI.Lam _ t f)         = FI.Fun t (this i (j+1) (f (j,t)))
infer' this i j (FI.BLam n f)          = FI.Forall n (\a -> FI.fsubstTT i (FI.TVar n a) $ this (i+1) j (f i))
infer' _    _ _ (FI.Fix _ _ _ t1 t)    = FI.Fun t1 t
infer' this i j (FI.Let _ b e)         = this i (j+1) (e (j, this i j b))
infer' this i j (FI.LetRec _ ts _ e)   = this i (j+n) (e (zip [j..j+n-1] ts)) where n = length ts
infer' this i j (FI.App f _)           = t12                where FI.Fun _ t12 = this i j f
infer' this i j (FI.TApp f t)          = FI.joinType ((unsafeCoerce g :: t -> FI.Type t) t) where FI.Forall _ g  = this i j f
infer' this i j (FI.If _ b1 _)         = this i j b1
infer' _    _ _ (FI.PrimOp _ op _)     = case op of S.Arith _   -> FI.JClass "java.lang.Integer"
                                                    S.Compare _ -> FI.JClass "java.lang.Boolean"
                                                    S.Logic _   -> FI.JClass "java.lang.Boolean"
infer' this i j (FI.Tuple es)          = FI.Product (map (this i j) es)
infer' this i j (FI.Proj index e)      = ts !! (index-1) where FI.Product ts = this i j e
infer' _    _ _ (FI.JNew c _)          = FI.JClass c
infer' _    _ _ (FI.JMethod _ _ _ c)   = FI.JClass c
infer' _    _ _ (FI.JField _ _ c)      = FI.JClass c
infer' this i j (FI.Seq es)            = this i j (last es)
infer' this i j (FI.Merge e1 e2)       = FI.And (this i j e1) (this i j e2)
infer' this i j (FI.RecordIntro (l,e)) = FI.Record (l, this i j e)
infer' this i j (FI.RecordElim e l1)   = t1 where Just (t1,_) = getter i j (this i j e) l1
infer' this i j (FI.RecordUpdate e _)  = this i j e
infer' this i j (FI.Case _ alts)       = inferAlt $ head alts
    where inferAlt (FI.ConstrAlt c _ e)  =
              let ts = FI.constrParams c
                  n = length ts
              in this i (j+n) (e (zip [j..j+n-1] ts))
infer' _ _ _ (FI.Constr c _)           = last $ FI.constrParams c

infer :: Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> FI.Type Index
infer = new infer'

instance (FI.Type Index, Expr Index Index) <: FI.Type Index where
  up = fst

transExpr'
  :: (Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> FI.Type Index)
  -> (Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> (FI.Type Index, Expr Index Index))
  -> Index  -> Index -> FI.Expr Index (Index, FI.Type Index) -> Expr Index Index
transExpr' _ _    _ _ (FI.Var n (x,_))      = Var n x
transExpr' _ _    _ _ (FI.Lit l)            = Lit l
transExpr' _ this i j (FI.Lam n t f)        = Lam n (transType i t) (\x -> fsubstEE j (Var n x) body')    where (_, body') = this i (j+1) (f (j, t))
transExpr' _ this i j (FI.BLam n f)         = BLam n (\a -> fsubstTE i (TVar n a) body')               where (_, body') = this (i+1) j (f i)
transExpr' _ this i j (FI.Fix n1 n2 f t1 t) = Fix n1 n2 (\x x1 -> (fsubstEE j (Var n1 x) . fsubstEE (j+1) (Var n2 x1)) body') t1' t'
  where
    (_, body') = this i (j+2) (f (j, FI.Fun t1 t) (j+1, t1))
    t1'        = transType i t1
    t'         = transType i t
transExpr' super this i j (FI.Let n b e) = Let n b' (\x -> fsubstEE j (Var n x) (snd (this i (j+1) (e (j, super i j b)))))
  where
    (_,b') = this i j b
transExpr' _     this i j (FI.LetRec ns ts bs e) = LetRec ns' ts' bs' e'
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
    subst xs rs   = foldl (.) id [fsubstEE x (Var (ns' !! k) (rs !! k)) | (x,k) <- zip xs [0..n-1]] -- right?

transExpr' _ this i j (FI.App e1 e2)
  = let (FI.Fun t11 t12, e1') = this i j e1
        (t2, e2')            = this i j e2
    in
    let panic_doc             = text "Coercion failed" <$>
                                text "Function:" <+> pretty_typing e1 (FI.Fun t11 t12) <$>
                                text "Argument:" <+> pretty_typing e2 t2 <$>
                                text "Coercion:" <+> pretty_coercion t2 t11
        pretty_typing e t     = FI.prettyExpr (unsafeCoerce e :: FI.Expr Index Index) <+> colon <+>
                                FI.prettyType (unsafeCoerce t :: FI.Type Index)
        pretty_coercion s1 s2 = FI.prettyType (unsafeCoerce s1 :: FI.Type Index) <+> text "<:" <+> FI.prettyType (unsafeCoerce s2 :: FI.Type Index)
    in
    let c = fromMaybe (prettyPanic "Simplify.transExpr'" panic_doc) (coerce i t2 t11)
    in App e1' (App c e2')

transExpr' _ this i j (FI.TApp e t)                   = TApp (snd (this i j e)) (transType i t)
transExpr' _ this i j (FI.If p b1 b2)                 = If (snd (this i j p)) (snd (this i j b1)) (snd (this i j b2))
transExpr' _ this i j (FI.PrimOp e1 op e2)            = PrimOp (snd (this i j e1)) op (snd (this i j e2))
transExpr' _ this i j (FI.Tuple es)                   = Tuple (snd (unzip (map (this i j) es)))
transExpr' _ this i j (FI.Proj index e)               = Proj index (snd (this i j e))
transExpr' _ this i j (FI.JNew c es)                  = JNew c (snd (unzip (map (this i j) es)))
transExpr' _ this i j (FI.Constr (FI.Constructor n ts) es) = Constr (Constructor n (map (transType i) ts)) (map (snd . this i j) es)
transExpr' _ this i j (FI.Case e alts)                = Case e' (map transAlt alts)
    where (_,e') = this i j e
          transAlt (FI.ConstrAlt (FI.Constructor n ts) ns f) =
              let m = length ts
                  js = [j..j+m-1]
                  (_,f') = this i (j+m) (f (zip js ts))
                  ts' = map (transType i) ts
              in ConstrAlt (Constructor n ts') ns (\es -> foldl (\acc (j',x) -> fsubstEE j' (var x) acc) f' (zip js es))

-- At the moment, in `System.out.println(x)`, `x` can be left as a thunk even
-- after the simplification. We need to recursively force the arguments of a
-- Java method call. The current solution is not very neat. It'd be better to
-- uniformly address all similar concerns for `App`, `JMethod`, and `PrimOp`.
transExpr' _ this i j (FI.JMethod callee m args ret)
  = let args' = map (forceLazy . this i j) args in
    JMethod (fmap (snd . this i j) callee) m args' ret
  where
    forceLazy (_,e)          = e

transExpr' _ this i j (FI.JField callee m ret)        = JField (fmap (snd . this i j) callee) m ret
transExpr' _ this i j (FI.Seq es)                     = Seq (snd (unzip (map (this i j) es)))
transExpr' _ this i j (FI.Merge e1 e2)                = Tuple [snd (this i j e1), snd (this i j e2)]
transExpr' _ this i j (FI.RecordIntro (_,e))          = snd (this i j e)
transExpr' super this i j (FI.RecordElim e l1)        = App c (snd (this i j e)) where Just (_,c) = getter i j (super i j e) l1
transExpr' super this i j (FI.RecordUpdate e (l1,e1)) = App c (snd (this i j e)) where Just (_,c) = putter i j (super i j e) l1 (snd (this i j e1))

transExpr :: Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> (FI.Type Index, Expr Index Index)
transExpr = new (infer' `with` transExpr'')
  where
    transExpr'' :: Mixin
      (Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> FI.Type Index)
      (Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> (FI.Type Index, Expr Index Index))
    transExpr'' super this i j e  = (super i j e, transExpr' super this i j e)

getter :: Index -> Index -> FI.Type Index -> S.Label -> Maybe (FI.Type Index, Expr Index Index)
getter i j (FI.Record (l,t)) l1
  | l1 == l   = Just (t, lam (transType i (FI.Record (l,t))) var)
  | otherwise = Nothing
getter i j (FI.And t1 t2) l
  = case getter i j t2 l of
      Just (t,c) ->
        Just (t, lam (transType i (FI.And t1 t2)) (App c . Proj 2 . var))
      Nothing    ->
        case getter i j t1 l of
          Nothing    -> Nothing
          Just (t,c) ->
            Just (t, lam (transType i (FI.And t1 t2)) (App c . Proj 1 . var))
getter _ _ _ _ = Nothing

putter :: Index -> Index -> FI.Type Index -> S.Label -> Expr Index Index -> Maybe (FI.Type Index, Expr Index Index)
putter i j (FI.Record (l,t)) l1 e
  | l1 == l   = Just (t, Simplify.const (transType i (FI.Record (l,t))) e)
  | otherwise = Nothing
putter i j (FI.And t1 t2) l e
  = case putter i j t2 l e of
      Just (t,c) ->
        Just (t, lam (transType i (FI.And t1 t2)) (\x -> Tuple [Proj 1 (var x), App c (Proj 2 (var x))]))
      Nothing    ->
        case putter i j t1 l e of
          Nothing    -> Nothing
          Just (t,c) ->
            Just (t, lam (transType i (FI.And t1 t2)) (\x -> Tuple [App c (Proj 1 (var x)), Proj 2 (var x)]))
putter _ _ _ _ _ = Nothing

wrap :: Expr t e -> Expr t e
wrap e = lam Unit (Prelude.const e)

force :: Expr t e -> Expr t e
force e = App e (Lit S.UnitLit)

const :: Type t -> Expr t e -> Expr t e
const t e = lam t (Prelude.const e)
