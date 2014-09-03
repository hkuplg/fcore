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

import Unsafe.Coerce
import Control.Monad.Identity
import Control.Monad.State

-- import Mixins

-- import Unsafe.Coerce

import Prelude hiding (pred)

simplify :: Expr t e -> Expr t e
simplify e = unsafeCoerce $ snd (evalState (transExpr (unsafeCoerce e)) 0)

transExpr:: Expr Int (Int, Type Int) -> State Int (Type Int, Expr Int Int)

transExpr (Var (x, t)) = return (t, Var x)

transExpr (Lit (S.Integer n)) = return (JClass "java.lang.Integer", Lit (S.Integer n))
transExpr (Lit (S.String s))  = return (JClass "java.lang.String", Lit (S.String s))
transExpr (Lit (S.Boolean b)) = return (JClass "java.lang.Boolean", Lit (S.Boolean b))
transExpr (Lit (S.Char c))    = return (JClass "java.lang.Character", Lit (S.Char c))

transExpr (BLam f) =
  do i <- takeFreshIndex
     (t, m) <- transExpr (f i)
     return ( Forall (\a -> fsubstTT (i, TVar a) t)
            , BLam (\a -> fsubstEE (i, Var a) m))

transExpr (Lam t f) =
  do i <- takeFreshIndex
     (t1, m) <- transExpr (f (i, t))
     return (Fun t t1, Lam (transType i t) (\x -> fsubstEE (i, Var x) m))

transExpr (TApp e t) =
  do i <- takeFreshIndex
     (t1, m) <- transExpr e
     case t1 of
       Forall f -> return (fsubstTT (i, t) (f i), TApp m (transType i t))
       _        -> panic "Simplify.transExpr: TApp"

transExpr (App e1 e2)  =
  do (t1, e1') <- transExpr e1
     case t1 of
        Fun t t'  ->
          do (t2, m2) <- transExpr e2
             i <- get
             case coerce i t2 t of
               Just c  -> return (t', e1' `App` (c `appC` m2))
               Nothing -> panic "Simplify.transExpr: App: Incompatible types"
        _            -> panic "Simplify.transExpr: App: Not a function"

transExpr (PrimOp e1 op e2) =
 do (t1, m1) <- transExpr e1
    (t2, m2) <- transExpr e2
    return (opReturnType op, PrimOp m1 op m2)

transExpr (If pred b1 b2) =
  do (predTy, pred') <- transExpr pred
     (b1Ty, b1')     <- transExpr b1
     (b2Ty, b2')     <- transExpr b2
     return (b1Ty, If pred' b1' b2')

transExpr (Tuple es) =
  do (ts, es') <- mapAndUnzipM transExpr es
     return (Product ts, Tuple es')

transExpr (Proj i e) =
  do (Product ts, e') <- transExpr e
     return (ts !! (i - 1), Proj i e')

transExpr (Merge e1 e2) =
  do (t1, e1') <- transExpr e1
     (t2, e2') <- transExpr e2
     return (And t1 t2, Tuple [e1', e2'])

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

transExpr (Fix f t1 t) =
  do i <- takeFreshIndex
     return ( t1 `Fun` t
            , Fix (\x x1 -> snd (evalState (transExpr (f (x, t1 `Fun` t) (x1, t1))) i))
                  (transType i t1)
                  (transType i t))

-- LetRec [(Type t, Type t)] ([e] -> [Expr t e]) ([e] -> Expr t e)
transExpr (LetRec sigs binds body) =
  do i <- takeFreshIndex
     let sigs' = map (\(t1,t2) -> (transType i t1, transType i t2)) sigs
     js <- replicateM (length sigs) takeFreshIndex
     k <- takeFreshIndex
     let binds' = (\names' ->
                    map (\e -> snd (evalState (transExpr e) k))
                      (binds (zipWith (\n (t1,t2) -> (n, Fun t1 t2)) names' sigs)))
     let body' = (\names' ->
                      snd (evalState (transExpr (body (zipWith (\n (t1,t2) -> (n, Fun t1 t2)) names' sigs))) k))

     let t = infer ((unsafeCoerce body') (map (\(t1,t2) -> Fun t1 t2) sigs))
     k  <- takeFreshIndex
     return (t, LetRec sigs' binds' body')

infer :: Expr t e -> Type t
infer e = unsafeCoerce $ fst (evalState (transExpr e') 0)
  where e' = unsafeCoerce e

transType :: Int -> Type Int -> Type Int
transType i (TVar a)      = TVar a
transType i (a1 `Fun` a2) = transType i a1 `Fun` transType i a2
transType i (Forall f)    = Forall (\a -> transType (i + 1) (f i)) -- bug!
transType i (Product ts)  = Product (map (transType i) ts)
transType i (JClass c)    = JClass c
transType i (a1 `And` a2) = Product [transType i a1, transType i a2]

-- A `Coercion` is either an identity function or some non-trivial function.
-- The purpose is to avoid applying the identity functions to an expression.
data Coercion t e = Id | C (Expr t e)

isIdC :: Coercion t e -> Bool
isIdC  Id   = True
isIdC (C _) = False

appC :: Coercion t e -> Expr t e -> Expr t e
appC Id e      = e
appC (C e1) e2 = App e1 e2

coerce :: Int -> Type Int -> Type Int -> Maybe (Coercion Int Int)
coerce i (TVar a) (TVar b)
  | a == b    = return Id
    -- TODO: requires variable identity
  | otherwise = Nothing
coerce i (Fun t1 t2) (Fun t3 t4) =
  do c1 <- coerce i t3 t1
     c2 <- coerce i t2 t4
     case (c1,c2) of
       (Id,Id) ->
         return Id
       (_,_)    ->
         return (C (Lam (transType i (Fun t1 t2))
                      (\f -> Lam (transType i t3)
                               (\x -> (appC c2 .  App (Var f) . appC c1) (Var x)))))
coerce i (Forall f) (Forall g) =
  do c <- coerce (i + 1) (f i) (g i)
     case c of
       Id -> return Id
       _  -> return (C (Lam (transType i (Forall f))
                         (\f -> BLam (\a -> (appC c . TApp (Var f)) (TVar a)))))
coerce i (Product ss) (Product ts)
  | length ss == length ts = Nothing
  | otherwise =
    do cs <- zipWithM (coerce i) ss ts
       if isIdC `all` cs
          then return Id
          else
            let f x = Tuple (zipWith (\c idx -> appC c (Proj idx x))
                                     cs [1.. (length ss)])
            in
            return (C (Lam (transType i (Product ss)) (\tuple -> f (Var tuple))))
coerce i (JClass c) (JClass d)
  | c == d    = return Id
  | otherwise = Nothing
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
coerce  i _ _ = Nothing

takeFreshIndex :: State Int Int
takeFreshIndex =
  do i <- get
     put (i + 1)
     return i

-- subtype :: Type Int -> Type Int -> Bool
-- subtype t1 t2 = runIdentity (new subtypeM 0 t1 t2)

-- subtypeM:: Monad m => Open (Int -> Type Int -> Type Int -> m Bool)
-- subtypeM this i (TVar a)    (TVar b)    = return (a == b)
-- subtypeM this i (Forall f)  (Forall g)  = this (i + 1)(f i) (g i)
-- subtypeM this i (Fun t1 t2) (Fun t3 t4) = liftM2 (&&) (this i t3 t1) (this i t2 t4)
-- subtypeM this i t              (And t1 t2) = liftM2 (&&) (this i t  t1) (this i t  t2)
-- subtypeM this i (And t1 t2) t              = liftM2 (||) (this i t1 t)  (this i t2 t)
-- subtypeM this i _           _                 = error "subtypeM"

-- infer :: Expr t e -> PrettyPHOAS (Type Int)
-- infer = PrettyPHOAS . runIdentity . new inferM 0 . unsafeCoerce

-- inferM:: Monad m => Open (Int -> Expr Int (Type Int) -> m (Type Int, Int))
-- inferM this i (Var x)       = return (x, i)
-- inferM this i (Lit (S.Integer _)) = return (JClass "java.lang.Integer", i)
-- inferM this i (Lit (S.String _))  = return (JClass "java.lang.String", i)
-- inferM this i (Lit (S.Boolean _)) = return (JClass "java.lang.Boolean", i)
-- inferM this i (Lit (S.Char _))    = return (JClass "java.lang.Character", i)
-- inferM this i (BLam f)      = do (t, _) <- this (i + 1) (f i)
--                                  return (Forall (\a -> fsubstTT (i, TVar a) t), i)
-- inferM this i (Lam t f)     = do (t', _) <- this i (f t)
--                                  return (Fun t t', i)
-- inferM this i (TApp e t)    = do (t', i') <- this i e
--                                  case t' of
--                                    Forall f -> return (fsubstTT (i', t) (f i'), i' + 1)
--                                    _        -> fail ""
-- inferM this i (App e1 e2)   = do (t1, _) <- this i e1
--                                  (t2, _) <- this i e2
--                                  case t1 of
--                                    Fun t t'| t2 `subtype` t -> return (t', i)
--                                    _                        -> fail ""
-- inferM this i (Merge e1 e2) = do (t1, _) <- this i e1
--                                  (t2, _) <- this i e2
--                                  return (And t1 t2, i)

-- transType2 :: Type t -> Type t
-- transType2 (TVar a)      = TVar a
-- transType2 (JClass c)    = JClass c
-- transType2 (a1 `Fun` a2) = transType2 a1 `Fun` transType2 a2
-- transType2 (a1 `And` a2) = Product [transType2 a1, transType2 a2]
-- transType2 (Forall f)    = Forall (transType2 . f)

-- data CoercionPrimitive
--   = IntOfNat
--   | TakeFst
--   | TakeSnd
--   -- f ↪ C2 (λ(x : t). f (C1 x))
--   | EtaExpand (Type Int, Coercion) -- C1
--               Coercion -- C2
--   | BigEtaExpand
--   | Clone Coercion Coercion
--   deriving (Show)

-- type Coercion = [CoercionPrimitive]

-- applyCoercion :: Coercion -> Expr Int Int -> Expr Int Int
-- applyCoercion [] e                        = e
-- applyCoercion (TakeFst:cs) e              = Proj 1 (applyCoercion cs e)
-- applyCoercion (TakeSnd:cs) e              = Proj 2 (applyCoercion cs e)
-- applyCoercion (EtaExpand (t, c1) c2:cs) e = applyCoercion c2 (Lam t (\x -> e `App` applyCoercion c1 (Var "" x)))
-- applyCoercion (BigEtaExpand:cs) e         = BLam (\a -> e `TApp` TVar a)
-- applyCoercion (Clone c1 c2:cs) e          = Tuple [applyCoercion c1 e, applyCoercion c2 e]

-- genCoercion :: Int -> Type Int -> Type Int -> Maybe Coercion
-- genCoercion i (TVar a) (TVar b) | a == b = return []
-- genCoercion i t           (And t1 t2) =
--   case genCoercion i t t1 of
--     Just c  -> return (TakeFst:c)
--     Nothing -> case genCoercion i t t2 of
--                  Just c  -> return (TakeSnd:c)
--                  Nothing -> Nothing
-- genCoercion i (And t1 t2) t =
--   do c1 <- genCoercion i t1 t
--      c2 <- genCoercion i t2 t
--      return [Clone c1 c2]
-- genCoercion i (Fun t1 t2) (Fun t3 t4) =
--   do c1 <- genCoercion i t3 t1
--      c2 <- genCoercion i t2 t4
--      return (if null c1 && null c2
--                 then []
--                 else [EtaExpand (transType i t3, c1) c2]) -- TODO
-- genCoercion i (Forall f) (Forall g)  =
--   do c <- genCoercion (i + 1) (f i) (g i)
--      return (if null c
--                 then []
--                 else BigEtaExpand:c)
-- genCoercion i _ _ = Nothing

-- liftMaybe :: (MonadPlus m) => Maybe a -> m a
-- liftMaybe = maybe mzero return