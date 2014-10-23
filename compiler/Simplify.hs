-- The simplifier: translate System F with intersection types to vanilla System F

{-# OPTIONS_GHC -Wall #-}

module Simplify where

import Core
import qualified Src as S

import Control.Monad (zipWithM)
import Unsafe.Coerce (unsafeCoerce)

simplify :: Expr t e -> Expr t e
simplify = unsafeCoerce . snd . transExpr 0 0 . unsafeCoerce

infer :: Expr t e -> Type t
infer = unsafeCoerce . fst . transExpr 0 0 . unsafeCoerce

transType :: Index -> Type Index -> Type Index
transType _ (TyVar a)        = TyVar a
transType _ (JClass c)       = JClass c
transType i (Fun a1 a2)      = Fun (transType i a1) (transType i a2)
transType i (Forall f)       = Forall (\a -> transType (i + 1) $ fsubstTT (i, TyVar a) (f i))
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

coerce :: Index -> Type Index -> Type Index -> Maybe (Coercion Index Index)
coerce _ (TyVar a) (TyVar b) | a == b    = return Id -- TODO: How about alpha equivalence?
                             | otherwise = Nothing
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
                               (\x -> (appC c2 .  App (Var f) . appC c1) (Var x)))))
coerce i (Forall f) (Forall g) =
  do c <- coerce (i + 1) (f i) (g i)
     case c of
       Id -> return Id
       _  -> return (C (Lam (transType i (Forall f))
                         (\f' -> BLam (\a -> (appC c . TApp (Var f')) (TyVar a)))))
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
            return (C (Lam (transType i (Product ss)) (\tuple -> f (Var tuple))))
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

transExpr:: Index -> Index -> Expr Index (Index, Type Index) -> (Type Index, Expr Index Index)
transExpr _ _ (Var (x, t))        = (t, Var x)
transExpr _ _ (Lit (S.Integer n)) = (JClass "java.lang.Integer",   Lit (S.Integer n))
transExpr _ _ (Lit (S.String s))  = (JClass "java.lang.String",    Lit (S.String s))
transExpr _ _ (Lit (S.Boolean b)) = (JClass "java.lang.Boolean",   Lit (S.Boolean b))
transExpr _ _ (Lit (S.Char c))    = (JClass "java.lang.Character", Lit (S.Char c))
transExpr _ _ (Lit  S.Unit)       = (JClass "java.lang.Integer",   Lit (S.Integer 0))
transExpr i j (Lam t f) = (Fun t tbody, Lam t' (\x -> fsubstEE (j, Var x) body'))
  where (tbody, body') = transExpr i (j+1) (f (j, t))
        t'             = transType i t
transExpr i j (Fix f t1 t) = (Fun t1 t, Fix (\x x1 -> (fsubstEE (j, Var x) . fsubstEE (j+1, Var x1)) body') t1' t')
  where (_, body') = transExpr i (j+2) (f (j, Fun t1 t) (j+1, t1))
        t1' = transType i t1
        t'  = transType i t
transExpr i j (LetRec ts bs e) = (tbody, LetRec ts' bs' e')
  where
    ts'  = map (transType i) ts
    bs' = (\fs' -> map (subst fs fs') opened_binds')
    e'  = (\fs' -> subst fs fs' opened_body')

    (_, opened_binds')     = unzip (map (transExpr i (j+n)) (bs fs_with_sigs))
    (tbody, opened_body')  = transExpr i (j+n) (e fs_with_sigs)

    n            = length ts
    fs           = [j..j+n-1]
    fs_with_sigs = zip fs ts

    subst :: [Index] -> [Index] -> Expr Index Index -> Expr Index Index
    subst xs rs  = foldl (.) id [fsubstEE (x, Var (rs !! k)) | (x,k) <- (zip xs [0..n-1])]
transExpr i j (BLam f) = (Forall (\a -> fsubstTT (i, TyVar a) tbody), BLam (\a -> fsubstTE (i, TyVar a) body'))
  where (tbody, body') = transExpr (i+1) j (f i)
transExpr i j (App e1 e2) = (t12, App e1' (c `appC` e2'))
  where
    (Fun t11 t12, e1') = transExpr i j e1
    (t2, e2')          = transExpr i j e2
    Just c             = coerce i t2 t11
transExpr i j (TApp e t1) = (fsubstTT (i, t1) (f i), TApp e' t1')
  where
    (Forall f, e') = transExpr i j e
    t1'            = transType i t1
transExpr i j (If p b1 b2) = (tb1, If p' b1' b2')
  where
    (_,   p')  = transExpr i j p
    (tb1, b1') = transExpr i j b1
    (_,   b2') = transExpr i j b2
transExpr i j (PrimOp e1 op e2) = (t, PrimOp e1' op e2')
  where
    (_, e1') = transExpr i j e1
    (_, e2') = transExpr i j e2
    t        = case op of
                 S.Arith _   -> JClass "java.lang.Integer"
                 S.Compare _ -> JClass "java.lang.Boolean"
                 S.Logic _   -> JClass "java.lang.Boolean"
transExpr i j (Tuple es) = (Product ts, Tuple es')
  where
    (ts, es') = unzip (map (transExpr i j) es)
transExpr i j (Proj index e) = (ts !! (index-1), Proj index e')
  where
    (Product ts, e') = transExpr i j e
transExpr i j (JNewObj c es) = (JClass c, JNewObj c es')
  where
    (_, es') = unzip (map (transExpr i j) es)
transExpr i j (JMethod obj m args ret) = (JClass ret, JMethod obj' m args' ret)
  where
    obj' = case obj of
             Left c  -> Left c
             Right e -> Right e' where (_, e') = transExpr i j e
    (_, args') = unzip (map (transExpr i j) args)
transExpr i j (JField obj m ret) = (JClass ret, JField obj' m ret)
  where
    obj' = case obj of
             Left c  -> Left c
             Right e -> Right e' where (_, e') = transExpr i j e
transExpr i j (Seq es) = (last ts, Seq es')
  where
    (ts, es') = unzip (map (transExpr i j) es)
transExpr i j (Merge e1 e2) = (And t1 t2, Tuple [e1', e2'])
  where
    (t1, e1') = transExpr i j e1
    (t2, e2') = transExpr i j e2
transExpr i j (Record (l,e)) = (RecordTy (l,t), e')
  where
    (t, e') = transExpr i j e
transExpr i j (RecordAccess e l1) = (t1, appC c e')
  where
    (t, e')      = transExpr i j e
    Just (c, t1) = getter i t l1
transExpr i j (RecordUpdate e (l1,e1)) = (t, appC c e')
  where
    (t,  e')    = transExpr i j e
    (_, e1')    = transExpr i j e1
    Just (c, _) = putter i t l1 e1'

getter :: Index -> Type Index -> S.Label -> Maybe (Coercion Index Index, Type Index)
getter _ (RecordTy (l,t)) l1
  | l1 == l   = Just (Id, t)
  | otherwise = Nothing
getter i (And t1 t2) l
  = case getter i t2 l of
      Just (c,t) ->
        Just (C $ Lam (transType i (And t1 t2)) (\x -> appC c (Proj 2 (Var x)))
             ,t)
      Nothing    ->
        case getter i t1 l of
          Nothing    -> Nothing
          Just (c,t) ->
            Just (C $ Lam (transType i (And t1 t2)) (\x -> appC c (Proj 1 (Var x)))
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
coreId t = Lam t (\x -> Var x)

-- Core's const, specialized to type t.
coreConst :: Type Index -> Expr Index Index -> Expr Index Index
coreConst t e = Lam t (const e)
