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
import Panic

import Text.PrettyPrint.Leijen

import Data.Maybe    (fromMaybe)
import Control.Monad (zipWithM)
import Unsafe.Coerce (unsafeCoerce)

simplify :: Expr t e -> Expr t e
simplify = unsafeCoerce . snd . transExpr 0 0 . unsafeCoerce

transType :: Index -> Type Index -> Type Index
transType _ (TVar a)       = TVar a
transType _ (JClass c)     = JClass c
transType i (Fun a1 a2)    = Fun (transType i a1) (transType i a2)
transType i (Forall f)     = Forall (\a -> transType (i + 1) $ fsubstTT i (TVar a) (f i))
transType i (Product ts)   = Product (map (transType i) ts)
transType _  Unit          = Unit
transType i (And a1 a2)    = Product [transType i a1, transType i a2]
transType i (Record (_,t)) = transType i t
transType i (Thunk t)      = Fun Unit (transType i t)

-- Subtyping

subtype' :: Class (Index -> Type Index -> Type Index -> Bool)
subtype' _    _ (TVar a)     (TVar b)    = a == b
subtype' _    _ (JClass c)   (JClass d)  = c == d
subtype' this i (Fun t1 t2)  (Fun t3 t4) = this i t3 t1 && this i t2 t4
subtype' this i (Forall f)   (Forall g)  = this (i+1) (f i) (g i)
subtype' this i (Product ss) (Product ts)
  | length ss /= length ts               = False
  | otherwise                            = uncurry (this i) `all` zip ss ts
subtype' _    _ Unit     Unit    = True
subtype' this i t1 (And t2 t3)           = this i t1 t2 && this i t1 t3
subtype' this i (And t1 t2) t3           = this i t1 t3 || this i t2 t3
subtype' this i (Record (l1,t1)) (Record (l2,t2))
  | l1 == l2                             = this i t1 t2
  | otherwise                            = False
subtype' this i t1           (Thunk t2)  = this i t1 t2
subtype' this i (Thunk t1)   t2          = this i t1 t2
subtype' _    _ _ _                      = False

subtype :: Index -> Type Index -> Type Index -> Bool
subtype = new subtype'

type Coercion t e = Expr t e

coerce :: Index -> Type Index -> Type Index -> Maybe (Coercion Index Index)
coerce i (TVar a) (TVar b) | a == b        = return (lam (transType i (TVar a)) var)
                           | otherwise     = Nothing
coerce i (JClass c) (JClass d) | c == d    = return (lam (transType i (JClass c)) var)
                               | otherwise = Nothing
coerce i (Fun t1 t2) (Fun t3 t4) =
  do c1 <- coerce i t3 t1
     c2 <- coerce i t2 t4
     return (lam (transType i (Fun t1 t2))
                 (\f -> lam (transType i t3) ((App c2 .  App (var f) . App c1) . var)))
coerce i (Forall f) (Forall g) =
  do c <- coerce (i + 1) (f i) (g i)
     return (lam (transType i (Forall f)) (\f' -> bLam ((App c . TApp (var f')) . TVar)))
coerce i (Product ss) (Product ts)
  | length ss /= length ts = Nothing
  | otherwise =
    do cs <- zipWithM (coerce i) ss ts
       let f x = Tuple (zipWith (\c idx -> App c (Proj idx x))
                                 cs [1..length ss])
       return (lam (transType i (Product ss)) (f . var))
coerce i Unit Unit = return (lam (transType i Unit) var)
coerce i t1 (And t2 t3) =
  do c1 <- coerce i t1 t2
     c2 <- coerce i t1 t3
     return (lam (transType i t1) (\x -> Tuple [App c1 (var x), App c2 (var x)]))
coerce i (And t1 t2) t3 =
  case coerce i t1 t3 of
    Just c  -> Just (lam (transType i (And t1 t2)) (App c . Proj 1 . var))
    Nothing ->
      case coerce i t2 t3 of
        Nothing -> Nothing
        Just c  -> return (lam (transType i (And t1 t2)) (App c . Proj 2 . var))
coerce i (Record (l1,t1)) (Record (l2,t2)) | l1 == l2  = coerce i t1 t2
                                           | otherwise = Nothing
coerce _ _ _ = Nothing

infer':: Class (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
infer' _    _ _ (Var _ (_,t))       = t
infer' _    _ _ (Lit (S.Int _))     = JClass "java.lang.Integer"
infer' _    _ _ (Lit (S.String _))  = JClass "java.lang.String"
infer' _    _ _ (Lit (S.Bool _))    = JClass "java.lang.Boolean"
infer' _    _ _ (Lit (S.Char _))    = JClass "java.lang.Character"
infer' _    _ _ (Lit  S.UnitLit)    = Unit
infer' this i j (Lam _ t f)         = Fun t (this i (j+1) (f (j,t)))
infer' this i j (BLam _ f)          = Forall (\a -> fsubstTT i (TVar a) $ this (i+1) j (f i))
infer' _    _ _ (Fix _ _ _ t1 t)    = Fun t1 t
infer' this i j (Let _ b e)         = this i (j+1) (e (j, this i j b))
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
infer' this i j (RecordLit (l,e))   = Record (l, this i j e)
infer' this i j (RecordElim e l1)   = t1 where Just (_,t1) = getter i (this i j e) l1
infer' this i j (RecordUpdate e _)  = this i j e
infer' this i j (Lazy e)            = Thunk (this i j e)

infer :: Index -> Index -> Expr Index (Index, Type Index) -> Type Index
infer = new infer'

instance (Type Index, Expr Index Index) <: Type Index where
  up = fst

transExpr'
  :: (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
  -> (Index -> Index -> Expr Index (Index, Type Index) -> (Type Int, Expr Index Index))
  -> Index  -> Index -> Expr Index (Index, Type Index) -> Expr Index Index
transExpr' _ _    _ _ (Var _ (x,_))     = var x
transExpr' _ _    _ _ (Lit l)           = Lit l
transExpr' _ this i j (Lam n t f)       = Lam n (transType i t) (\x -> fsubstEE j (var x) body') where (_, body') = this i     (j+1) (f (j, t))
transExpr' _ this i j (BLam n f)        = BLam n (\a -> fsubstTE i (TVar a) body')               where (_, body') = this (i+1) j     (f i)
transExpr' _ this i j (Fix n1 n2 f t1 t)      = Fix n1 n2 (\x x1 -> (fsubstEE j (var x) . fsubstEE (j+1) (var x1)) body') t1' t'
  where
    (_, body') = this i (j+2) (f (j, Fun t1 t) (j+1, t1))
    t1'        = transType i t1
    t'         = transType i t
transExpr' super this i j (Let n b e) = Let n b' (\x -> fsubstEE j (var x) (snd (this i (j+1) (e (j, super i j b)))))
  where
    (_,b') = this i j b
transExpr' _     this i j (LetRec ts bs e) = LetRec ts' bs' e'
  where
    ts'           = map (transType i) ts
    bs' fs'       = map (subst fs fs') bs_body'
    e'  fs'       = subst fs fs' e_body'
    (_, bs_body') = unzip (map (transExpr i (j+n)) (bs fs_with_ts))
    (_, e_body')  = this i (j+n) (e fs_with_ts)
    fs            = [j..j+n-1]
    fs_with_ts    = zip fs ts
    n             = length ts
    subst :: [Index] -> [Index] -> Expr Index Index -> Expr Index Index
    subst xs rs   = foldl (.) id [fsubstEE x (var (rs !! k)) | (x,k) <- zip xs [0..n-1]]

transExpr' _ this i j (App e1 e2)
  = let (Fun t11 t12, e1') = this i j e1
        (t2, e2')          = this i j e2
    in
    let panic_doc             = text "Coercion failed" <$>
                                text "Function:" <+> pretty_typing e1 (Fun t11 t12) <$>
                                text "Argument:" <+> pretty_typing e2 t2 <$>
                                text "Coercion:" <+> pretty_coercion t2 t11
        pretty_typing e t     = prettyExpr (unsafeCoerce e :: Expr Index Index) <+> colon <+>
                                prettyType (unsafeCoerce t :: Type Index)
        pretty_coercion s1 s2 = prettyType (unsafeCoerce s1 :: Type Index) <+> text "<:" <+> prettyType (unsafeCoerce s2 :: Type Index)
    in
    case t11 of
      Thunk t11_naked ->
        let c = fromMaybe (prettyPanic "Simplify.transExpr'" panic_doc) (coerce i t2 t11_naked)
        in App e1' (wrap (App c e2'))
      _       ->
        let c = fromMaybe (prettyPanic "Simplify.transExpr'" panic_doc) (coerce i t2 t11)
        in App e1' (App c e2')

transExpr' _ this i j (TApp e t)                   = TApp (snd (this i j e)) (transType i t)
transExpr' _ this i j (If p b1 b2)                 = If (snd (this i j p)) (snd (this i j b1)) (snd (this i j b2))
transExpr' _ this i j (PrimOp e1 op e2)            = PrimOp (snd (this i j e1)) op (snd (this i j e2))
transExpr' _ this i j (Tuple es)                   = Tuple (snd (unzip (map (this i j) es)))
transExpr' _ this i j (Proj index e)               = Proj index (snd (this i j e))
transExpr' _ this i j (JNew c es)                  = JNew c (snd (unzip (map (this i j) es)))

-- At the moment, in `System.out.println(x)`, `x` can be left as a thunk even
-- after the simplification. We need to recursively force the arguments of a
-- Java method call. The current solution is not very neat. It'd be better to
-- uniformly address all similar concerns for `App`, `JMethod`, and `PrimOp`.
transExpr' _ this i j (JMethod callee m args ret)
  = let args' = map (forceLazy . this i j) args in
    JMethod (fmap (snd . this i j) callee) m args' ret
  where
    forceLazy (Thunk _, e) = force e
    forceLazy (_,e)        = e

transExpr' _ this i j (JField callee m ret)        = JField (fmap (snd . this i j) callee) m ret
transExpr' _ this i j (Seq es)                     = Seq (snd (unzip (map (this i j) es)))
transExpr' _ this i j (Merge e1 e2)                = Tuple [snd (this i j e1), snd (this i j e2)]
transExpr' _ this i j (RecordLit (_,e))            = snd (this i j e)
transExpr' super this i j (RecordElim e l1)        = App c (snd (this i j e)) where Just (c, _) = getter i (super i j e) l1
transExpr' super this i j (RecordUpdate e (l1,e1)) = App c (snd (this i j e)) where Just (c, _) = putter i (super i j e) l1 (snd (this i j e1))
transExpr' _ this i j (Lazy e)                     = lam Unit (\_ -> snd (this i j e))

transExpr :: Index -> Index -> Expr Index (Index, Type Index) -> (Type Index, Expr Index Index)
transExpr = new (infer' `with` transExpr'')
  where
    transExpr'' :: Mixin
      (Index -> Index -> Expr Index (Index, Type Index) -> Type Index)
      (Index -> Index -> Expr Index (Index, Type Index) -> (Type Index, Expr Index Index))
    transExpr'' super this i j e  = (super i j e, transExpr' super this i j e)

getter :: Index -> Type Index -> S.Label -> Maybe (Expr Index Index, Type Index)
getter i (Record (l,t)) l1
  | l1 == l   = Just (lam (transType i (Record (l,t))) var, t)
  | otherwise = Nothing
getter i (And t1 t2) l
  = case getter i t2 l of
      Just (c,t) ->
        Just (lam (transType i (And t1 t2)) (App c . Proj 2 . var), t)
      Nothing    ->
        case getter i t1 l of
          Nothing    -> Nothing
          Just (c,t) ->
            Just (lam (transType i (And t1 t2)) (App c . Proj 1 . var), t)
getter i (Thunk t1) l
  = do (c,t) <- getter i t1 l
       return (lam (transType i (Thunk t1)) (App c . force . var), t)
getter _ _ _ = Nothing

putter :: Index -> Type Index -> S.Label -> Expr Index Index -> Maybe (Expr Index Index, Type Index)
putter i (Record (l,t)) l1 e
  | l1 == l   = Just (Simplify.const (transType i (Record (l,t))) e, t)
  | otherwise = Nothing
putter i (And t1 t2) l e
  = case putter i t2 l e of
      Just (c,t) ->
        Just (lam (transType i (And t1 t2)) (\x -> Tuple [Proj 1 (var x), App c (Proj 2 (var x))]), t)
      Nothing    ->
        case putter i t1 l e of
          Nothing    -> Nothing
          Just (c,t) ->
            Just (lam (transType i (And t1 t2)) (\x -> Tuple [App c (Proj 1 (var x)), Proj 2 (var x)]), t)
putter i (Thunk t1) l e
  = do (c,t) <- putter i t1 l e
       return (lam (transType i (Thunk t1)) (App c . force . var), t)
putter _ _ _ _ = Nothing

wrap :: Expr t e -> Expr t e
wrap e = lam Unit (Prelude.const e)

force :: Expr t e -> Expr t e
force e = App e (Lit S.UnitLit)

-- Core's const, specialized to type t.
const :: Type t -> Expr t e -> Expr t e
const t e = Lam "_" t (Prelude.const e)
