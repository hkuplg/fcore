{-# LANGUAGE RankNTypes #-}
module PartialEvaluator (rewriteAndEval, Exp(..))where

import Core
import OptiUtils
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.List (foldl')
import Panic

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


-- TODO: refactor to reduce duplicate code
-- Rule 2: let x = e in x                 =>  e
rewrite2 :: Expr t Int -> Expr t e
rewrite2 = rewrite2' 0 Map.empty

rewrite2' :: Int -> Map.Map Int e -> Expr t Int -> Expr t e
rewrite2' num env (Let n expr f) = case f num of
                                    Var _ _ -> rewrite2' (num + 1) env expr
                                    e -> Let n (rewrite2' (num + 1) env expr) (\b -> rewrite2' (num + 2) (Map.insert num b env) e)
rewrite2' _ env (Var n v) = Var n (fromJust $ Map.lookup v env)
rewrite2' num env (App e1 e2) = App (rewrite2' num env e1) (rewrite2' (num + 1) env e2)
rewrite2' num env (PrimOp e1 op e2) = PrimOp (rewrite2' num env e1) op (rewrite2' (num + 1) env e2)
rewrite2' num env (Lam n t f) = Lam n t (\e -> rewrite2' (num + 1) (Map.insert num e env) (f num))
rewrite2' _ _ (Lit n) = Lit n
rewrite2' num env (Fix n1 n2 f t1 t2) =
  Fix n1 n2 (\a b -> rewrite2' (num + 2) (Map.insert (num + 1) b (Map.insert num a env)) (f num (num + 1))) t1 t2
rewrite2' num env (LetRec n t b f) =
  LetRec n t (\bs ->
               let len = length bs
                   bs' = b [num .. num + len - 1]
               in map (rewrite2' (num + len) (foldl' multInsert env (zip [num .. num + len -1] bs))) bs')
             (\es ->
               let len = length es
                   es' = f [num .. num + len - 1]
               in rewrite2' (num + len) (foldl' multInsert env (zip [num .. num + len - 1] es)) es')
rewrite2' num env (BLam n f) = BLam n (rewrite2' num env . f)
rewrite2' num env (TApp e t) = TApp (rewrite2' num env e) t
rewrite2' num env (If e1 e2 e3) = If (rewrite2' num env e1) (rewrite2' num env e2) (rewrite2' num env e3)
rewrite2' num env (Tuple es) = Tuple (map (rewrite2' num env) es)
rewrite2' num env (Proj n e) = Proj n (rewrite2' num env e)
rewrite2' num env (JNew n es) = JNew n (map (rewrite2' num env) es)
rewrite2' num env (JMethod e b es d) =
  JMethod (fmap (rewrite2' num env) e) b (map (rewrite2' num env) es) d
rewrite2' num env (JField e a b) = JField (fmap (rewrite2' num env) e) a b
rewrite2' num env (Seq es) = Seq (map (rewrite2' num env) es)
rewrite2' num env (Constr c es) = Constr c (map (rewrite2' num env) es)
rewrite2' _ _ (Case _ _) = sorry "Haven't implement case yet in rewriting"

multInsert :: Map.Map Int e -> (Int, e) -> Map.Map Int e
multInsert m (key, value) = Map.insert key value m



-- Rule 3: (\x . y x)                     => y (where y is a variable)
rewrite3 :: Expr t Int -> Expr t e
rewrite3 = rewrite3' 0 Map.empty

rewrite3' :: Int -> Map.Map Int e -> Expr t Int -> Expr t e
rewrite3' num env (Lam n t f) = case f num of
                                 (App (Var n' y) (Var _ _)) -> Var n' (fromJust $ Map.lookup y env)
                                 e' -> Lam n t (\e -> rewrite3' (num + 1) (Map.insert num e env) e')
rewrite3' num env (Let n expr f) =
  Let n (rewrite3' (num + 1) env expr) (\b -> rewrite3' (num + 2) (Map.insert num b env) (f num))
rewrite3' _ env (Var n v) = Var n (fromJust $ Map.lookup v env)
rewrite3' num env (App e1 e2) = App (rewrite3' num env e1) (rewrite3' (num + 1) env e2)
rewrite3' num env (PrimOp e1 op e2) = PrimOp (rewrite3' num env e1) op (rewrite3' (num + 1) env e2)
rewrite3' _ _ (Lit n) = Lit n
rewrite3' num env (Fix n1 n2 f t1 t2) =
  Fix n1 n2 (\a b -> rewrite3' (num + 2) (Map.insert (num + 1) b (Map.insert num a env)) (f num (num + 1))) t1 t2
rewrite3' num env (LetRec n t b f) =
  LetRec n t (\bs ->
               let len = length bs
                   bs' = b [num .. num + len - 1]
               in map (rewrite3' (num + len) (foldl' multInsert env (zip [num .. num + len -1] bs))) bs')
             (\es ->
               let len = length es
                   es' = f [num .. num + len - 1]
               in rewrite3' (num + len) (foldl' multInsert env (zip [num .. num + len - 1] es)) es')
rewrite3' num env (BLam n f) = BLam n (rewrite3' num env . f)
rewrite3' num env (TApp e t) = TApp (rewrite3' num env e) t
rewrite3' num env (If e1 e2 e3) = If (rewrite3' num env e1) (rewrite3' num env e2) (rewrite3' num env e3)
rewrite3' num env (Tuple es) = Tuple (map (rewrite3' num env) es)
rewrite3' num env (Proj n e) = Proj n (rewrite3' num env e)
rewrite3' num env (JNew n es) = JNew n (map (rewrite3' num env) es)
rewrite3' num env (JMethod e b es d) =
  JMethod (fmap (rewrite3' num env) e) b (map (rewrite3' num env) es) d
rewrite3' num env (JField e a b) = JField (fmap (rewrite3' num env) e) a b
rewrite3' num env (Seq es) = Seq (map (rewrite3' num env) es)
rewrite3' num env (Constr c es) = Constr c (map (rewrite3' num env) es)
rewrite3' _ _ (Case _ _) = sorry "Haven't implement case yet in rewriting"

peval :: Expr t e -> Expr t e
peval = rewrite1 .  app2let

newtype Exp = Hide {reveal :: forall t e. Expr t e}

partialEval :: Exp -> Exp
partialEval e = Hide (peval (reveal e))

rewrite :: Exp -> Exp
rewrite e = Hide (rewrite3 . rewrite2 . reveal $ e)

rewriteAndEval :: Exp -> Expr t e
rewriteAndEval = reveal . rewrite . partialEval

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
