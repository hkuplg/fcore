{-# LANGUAGE RankNTypes #-}
module PartialEvaluator (rewriteAndEval, Exp(..), rewrite1, rewrite2, rewrite3) where

import Core
import OptiUtils
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

app2let :: Expr t e -> Expr t e
app2let (App e1 e2) = case e1' of
  Lam _ _ f    -> Let "_" e2' f
  Let n body f -> Let n body (\x -> app2let (App (f x) e2'))
  _            -> App e1' e2'
  where
    e1' = app2let e1
    e2' = app2let e2
app2let e = mapExpr app2let e

-- Rule 1: let x = y in e                 => [x -> y] e   (where y is a variable)
rewrite1 :: Expr t e -> Expr t e
rewrite1 (Let _ (Var _ n) f) = rewrite1 (f n)
rewrite1 e = mapExpr rewrite1 e


-- Rule 2: let x = e in x                 =>  e
rewrite2 :: Expr t Int -> Expr t e
rewrite2 = rewrite2' 0 Map.empty

rewrite2' :: Int -> Map.Map Int e -> Expr t Int -> Expr t e
rewrite2' num env (Let n expr f) = case f num of
                                    e@(Var _ num') ->
                                      if num' == num
                                      then rewrite2' num env expr
                                      else Let n (rewrite2' num env expr) (\b -> rewrite2' (num + 1) (Map.insert num b env) e)
                                    e -> Let n (rewrite2' num env expr) (\b -> rewrite2' (num + 1) (Map.insert num b env) e)
rewrite2' num env e = rewriteExpr rewrite2' num env e

-- Rule 3: (\x . y x)                     => y (where y is a variable)
rewrite3 :: Expr t Int -> Expr t e
rewrite3 = rewrite3' 0 Map.empty

rewrite3' :: Int -> Map.Map Int e -> Expr t Int -> Expr t e
rewrite3' num env (Lam n t f) = case f num of
                                 e'@(App (Var n' y) (Var _ num')) ->
                                   if num == num'
                                   then Var n' (fromJust $ Map.lookup y env)
                                   else Lam n t (\e -> rewrite3' (num + 1) (Map.insert num e env) e')
                                 e' -> Lam n t (\e -> rewrite3' (num + 1) (Map.insert num e env) e')
rewrite3' num env e = rewriteExpr rewrite3' num env e

peval :: Expr t e -> Expr t e
peval = rewrite1 .  app2let

newtype Exp = Hide {reveal :: forall t e. Expr t e}

partialEval :: Exp -> Exp
partialEval e = Hide (peval (reveal e))

rewrite :: Exp -> Exp
rewrite e = Hide (rewrite3 . rewrite2 . reveal $ e)

rewriteAndEval :: Exp -> Expr t e
rewriteAndEval = reveal . rewrite . partialEval
-- rewriteAndEval = reveal . partialEval

-- calc :: Expr t (Expr t e) -> Expr t (Expr t e)
-- calc (App e1 e2) =
--     case (e1', e2') of
--       (Lam _ _ f, Lit _) -> calc . f . joinExpr $ e2'
--       _ -> App e1' e2'
--     where e1' = calc e1
--           e2' = calc e2
-- calc (Let n bind body) =
--     case bind' of
--       Lit _ -> calc . body . joinExpr $ bind'
--       _ -> Let n bind' (\x -> calc . body $ x)
--      where bind' = calc bind
-- calc (PrimOp e1 op e2) =
--     case (e1', e2') of
--       (Lit (S.Int a), Lit (S.Int b)) ->
--           case op of
--             -- arithmetic operations
--             S.Arith J.Sub -> Lit . S.Int $ a - b
--             S.Arith J.Add -> Lit . S.Int $ a + b
--             S.Arith J.Mult -> Lit . S.Int $ a * b
--             S.Arith J.Div -> Lit . S.Int $ a `div` b
--             S.Arith J.Rem -> Lit . S.Int $ a `rem` b
--             -- comparison operations
--             S.Compare J.Equal -> Lit . S.Bool $ a == b
--             S.Compare J.NotEq -> Lit . S.Bool $ a /= b
--             S.Compare J.LThan -> Lit . S.Bool $ a < b
--             S.Compare J.LThanE -> Lit . S.Bool $ a <= b
--             S.Compare J.GThan -> Lit . S.Bool $ a > b
--             S.Compare J.GThanE -> Lit . S.Bool $ a >= b
--             _ -> simplified
--       (Lit (S.Bool a), Lit (S.Bool b)) ->
--           case op of
--             -- logic operations
--             S.Logic J.CAnd -> Lit . S.Bool $ a && b
--             S.Logic J.COr -> Lit . S.Bool $ a || b
--             _ -> simplified
--       _ -> simplified
--     where e1' = calc e1
--           e2' = calc e2
--           simplified = PrimOp e1' op e2'
-- calc (If e1 e2 e3) =
--     case e1' of
--       Lit (S.Bool True) -> calc e2
--       Lit (S.Bool False) -> calc e3
--       _ -> If e1' (calc e2) (calc e3)
--     where e1' = calc e1
-- calc e = mapExpr calc e
