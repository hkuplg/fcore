module PartialEvaluator where

import Core
import OptiUtils
import qualified Src as S
import qualified Language.Java.Syntax as J (Op(..))

app2let :: Int -> Expr t (Expr t e) -> Expr t (Expr t e)
app2let i (App e1 e2) =
    case e1' of
      Lam n _ f -> Let ("temp" ++ show (i + 1)) e2' f
      Let n body f -> Let n body (\x -> app2let (i + 1) (App (f x) e2'))
      _ -> App e1' e2'
    where e1' = app2let (i + 1) e1
          e2' = app2let (i + 1) e2
app2let i e = mapExpr (app2let (i + 1)) e

calc :: Expr t (Expr t e) -> Expr t (Expr t e)
calc (App e1 e2) =
    case (e1', e2') of
      (Lam _ _ f, Lit _) -> calc . f . joinExpr $ e2'
      _ -> App e1' e2'
    where e1' = calc e1
          e2' = calc e2
calc (Let n bind body) =
    case bind' of
      Lit _ -> calc . body . joinExpr $ bind'
      _ -> Let n bind' (\x -> calc . body $ x)
     where bind' = calc bind
calc (PrimOp e1 op e2) =
    case (e1', e2') of
      (Lit (S.Int a), Lit (S.Int b)) ->
          case op of
            -- arithmetic operations
            S.Arith J.Sub -> Lit . S.Int $ a - b
            S.Arith J.Add -> Lit . S.Int $ a + b
            S.Arith J.Mult -> Lit . S.Int $ a * b
            S.Arith J.Div -> Lit . S.Int $ a `div` b
            S.Arith J.Rem -> Lit . S.Int $ a `rem` b
            -- comparison operations
            S.Compare J.Equal -> Lit . S.Bool $ a == b
            S.Compare J.NotEq -> Lit . S.Bool $ a /= b
            S.Compare J.LThan -> Lit . S.Bool $ a < b
            S.Compare J.LThanE -> Lit . S.Bool $ a <= b
            S.Compare J.GThan -> Lit . S.Bool $ a > b
            S.Compare J.GThanE -> Lit . S.Bool $ a >= b
            _ -> simplified
      (Lit (S.Bool a), Lit (S.Bool b)) ->
          case op of
            -- logic operations
            S.Logic J.CAnd -> Lit . S.Bool $ a && b
            S.Logic J.COr -> Lit . S.Bool $ a || b
            _ -> simplified
      _ -> simplified
    where e1' = calc e1
          e2' = calc e2
          simplified = PrimOp e1' op e2'
calc (If e1 e2 e3) =
    case e1' of
      Lit (S.Bool True) -> calc e2
      Lit (S.Bool False) -> calc e3
      _ -> If e1' (calc e2) (calc e3)
    where e1' = calc e1
calc e = mapExpr calc e

peval :: Expr t (Expr t e) -> Expr t e
peval = joinExpr . (app2let 0) -- . calc
