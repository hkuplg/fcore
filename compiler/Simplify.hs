-- The simplifier: translate System F with intersection types to vanilla System F

{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-matches #-}

module Simplify where

import Core
import qualified Src as S

import Panic
import PrettyUtils

import Unsafe.Coerce
import Control.Monad.Identity
import Control.Monad.State

import Prelude hiding (pred, id, const)

simplify :: Expr t e -> Expr t e
simplify e = unsafeCoerce $ snd (evalState (transExpr (unsafeCoerce e)) 0)

infer :: Expr t e -> Type t
infer e = unsafeCoerce $ fst (evalState (transExpr e') 0)
  where e' = unsafeCoerce e

-- Type translation

transType :: Int -> Type Int -> Type Int
transType i (TyVar a)        = TyVar a
transType i (JClass c)       = JClass c
transType i (Fun a1 a2)      = Fun (transType i a1) (transType i a2)
transType i (Forall f)       = Forall (\a -> transType (i + 1) (f i)) -- bug!
transType i (Product ts)     = Product (map (transType i) ts)
transType i (And a1 a2)      = Product [transType i a1, transType i a2]
transType i (RecordTy (l,t)) = transType i t

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

coerce :: Int -> Type Int -> Type Int -> Maybe (Coercion Int Int)
coerce i (TyVar a) (TyVar b) | a == b    = return Id -- TODO: How about alpha equivalence?
                             | otherwise = Nothing
coerce i (JClass c) (JClass d) | c == d    = return Id
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
                         (\f -> BLam (\a -> (appC c . TApp (Var f)) (TyVar a)))))
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
coerce i (RecordTy (l1,t1)) (RecordTy (l2,t2)) = coerce i t1 t2
coerce  i _ _ = Nothing

-- Typing

transExpr:: Expr Int (Int, Type Int) -> State Int (Type Int, Expr Int Int)

transExpr (Var (x, t)) = return (t, Var x)

transExpr (Lit (S.Integer n)) = return (JClass "java.lang.Integer", Lit (S.Integer n))
transExpr (Lit (S.String s))  = return (JClass "java.lang.String", Lit (S.String s))
transExpr (Lit (S.Boolean b)) = return (JClass "java.lang.Boolean", Lit (S.Boolean b))
transExpr (Lit (S.Char c))    = return (JClass "java.lang.Character", Lit (S.Char c))

transExpr (Lam t f) =
  do i <- takeFreshIndex
     (t1, m) <- transExpr (f (i, t))
     return (Fun t t1, Lam (transType i t) (\x -> fsubstEE (i, Var x) m))

transExpr (Fix f t1 t) =
  do i <- takeFreshIndex
     return ( t1 `Fun` t
            , Fix (\x x1 -> snd (evalState (transExpr (f (x, t1 `Fun` t) (x1, t1))) i))
                  (transType i t1)
                  (transType i t))

-- LetRec [Type t] ([e] -> [Expr t e]) ([e] -> Expr t e)
transExpr (LetRec sigs binds body) =
  do i <- takeFreshIndex
     let sigs' = map (transType i) sigs
     js <- replicateM (length sigs) takeFreshIndex
     k <- takeFreshIndex
     let binds' ids' = map (\e -> snd (evalState (transExpr e) k)) (binds (zipWith (\n t -> (n, t)) ids' sigs))
     let body' ids' = snd (evalState (transExpr (body (zipWith (\n t -> (n, t)) ids' sigs))) k)
     let t = infer (unsafeCoerce body' sigs)
     k <- takeFreshIndex
     return (t, LetRec sigs' binds' body')

transExpr (BLam f) =
  do i <- takeFreshIndex
     (t, m) <- transExpr (f i)
     return (Forall (\a -> fsubstTT (i, TyVar a) t), BLam (\a -> fsubstEE (i, Var a) m))

transExpr (App e1 e2)  =
  do (t1, e1') <- transExpr e1
     case t1 of
        Fun t11 t12  ->
          do (t2, e2') <- transExpr e2
             i <- get
             case coerce i t2 t11 of
               Just c  -> return (t12, e1' `App` (c `appC` e2'))
               Nothing -> panic ("Simplify.transExpr: App: Cannot coerce " ++
                                 show (pprType basePrec i t2) ++ " to " ++
                                 show (pprType basePrec i t11))
        _            -> panic "Simplify.transExpr: App: Not a function"

transExpr (TApp e t) =
  do i <- takeFreshIndex
     (t1, m) <- transExpr e
     case t1 of
       Forall f -> return (fsubstTT (i, t) (f i), TApp m (transType i t))
       _        -> panic "Simplify.transExpr: TApp"

transExpr (If pred b1 b2) =
  do (predTy, pred') <- transExpr pred
     (b1Ty, b1')     <- transExpr b1
     (b2Ty, b2')     <- transExpr b2
     return (b1Ty, If pred' b1' b2')

transExpr (PrimOp e1 op e2) =
 do (t1, m1) <- transExpr e1
    (t2, m2) <- transExpr e2
    return (opReturnType op, PrimOp m1 op m2)

transExpr (Tuple es) =
  do (ts, es') <- mapAndUnzipM transExpr es
     return (Product ts, Tuple es')

transExpr (Proj i e) =
  do (Product ts, e') <- transExpr e
     return (ts !! (i - 1), Proj i e')

transExpr (JNewObj c es) =
  do (ts, es') <- mapAndUnzipM transExpr es
     return (JClass c, JNewObj c es')

transExpr (JMethod (Right e) m args retC) =
  do (t, e') <- transExpr e
     (argsTys, args') <- mapAndUnzipM transExpr args
     return (JClass retC, JMethod (Right e') m args' retC)

transExpr (JMethod (Left c) m args retC) =
  do (argsTys, args') <- mapAndUnzipM transExpr args
     return (JClass retC, JMethod (Left c) m args' retC)

transExpr (JField (Right e) m retC) =
  do (t, e') <- transExpr e
     return (JClass retC, JField (Right e') m retC)

transExpr (JField (Left c) m retC) =
  return (JClass retC, JField (Left c) m retC)

transExpr (Seq es) =
  do (ts, es') <- mapAndUnzipM transExpr es
     return (last ts, Seq es')

transExpr (Merge e1 e2) =
  do (t1, e1') <- transExpr e1
     (t2, e2') <- transExpr e2
     return (And t1 t2, Tuple [e1', e2'])

transExpr (Record (l,e)) =
  do (t, e') <- transExpr e
     return (RecordTy (l,t), e')

transExpr (RecordAccess e l) =
  do (t, e') <- transExpr e
     i <- get
     case getter i t l of
       Nothing     -> panic "Simplify.transExpr:RecordAccess"
       Just (c,t1) -> return (t1, appC c e')

transExpr (RecordUpdate e (l1,e1)) =
  do (t,  e')  <- transExpr e
     (t1, e1') <- transExpr e1
     i <- get
     case putter i t l1 e1' of
       Nothing -> panic "Simplify.transExpr:RecordUpdate"
       Just (c,t2) ->
         -- Since the simplifier assumes everything passed in typechecks,
         -- the subtyping condition t1 <: t2 is not checked here.
         return (t, appC c e')

getter :: Int -> Type Int -> S.Label -> Maybe (Coercion Int Int, Type Int)
getter i (RecordTy (l,t)) l1
  | l1 == l   = Just (C $ id (transType i $ RecordTy (l,t)), t)
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

putter :: Int -> Type Int -> S.Label -> Expr Int Int -> Maybe (Coercion Int Int, Type Int)
putter i (RecordTy (l,t)) l1 e
  | l1 == l   = Just (C $ const (transType i $ RecordTy (l,t)) e, t)
  | otherwise = Nothing
putter i (And t1 t2) l e
  = case putter i t2 l e of
      Just (c,t) ->
        Just (C $ Lam (transType i (And t1 t2))
               (\x -> Tuple [Proj 1 (Var x), appC c (Proj 2 (Var x))])
             ,t)
      Nothing    ->
        case putter i t1 l e of
          Nothing    -> Nothing
          Just (c,t) ->
            Just (C $ Lam (transType i (And t1 t2))
                   (\x -> Tuple [appC c (Proj 1 (Var x)), Proj 2 (Var x)])
                 ,t)

-- id_t: the identity function in the Core world, specialised to type t.
id :: Type Int -> Expr Int Int
id t = Lam t (\x -> Var x)

-- const_t: the const function in the Core world, specialised to type t.
const :: Type Int -> Expr Int Int -> Expr Int Int
const t e = Lam t (\_ -> e)

takeFreshIndex :: State Int Int
takeFreshIndex =
  do i <- get
     put (i + 1)
     return i