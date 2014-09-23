module PartialEvaluator where

import Core
import OptiUtils
import qualified Src as S
import qualified Language.Java.Syntax as J (Op(..))

betaReduct :: Expr t (Expr t e) -> Expr t (Expr t e)
betaReduct (App e1 e2) =
    case e1' of
      Lam _ f -> f $ joinExpr e2'
      _ -> App e1' e2'
    where e1' = betaReduct e1
          e2' = betaReduct e2
betaReduct e = mapExpr betaReduct e

calc :: Expr t e -> Expr t e
calc (PrimOp e1 op e2) =
    case (e1', e2') of
      (Lit (S.Integer a), Lit (S.Integer b)) ->
          case op of
            -- arithmetic operations
            S.Arith J.Sub -> Lit . S.Integer $ a - b
            S.Arith J.Add -> Lit . S.Integer $ a + b
            S.Arith J.Mult -> Lit . S.Integer $ a * b
            S.Arith J.Div -> Lit . S.Integer $ a `div` b
            S.Arith J.Rem -> Lit . S.Integer $ a `rem` b
            -- comparison operations
            S.Compare J.Equal -> Lit . S.Boolean $ a == b
            S.Compare J.NotEq -> Lit . S.Boolean $ a /= b
            S.Compare J.LThan -> Lit . S.Boolean $ a < b
            S.Compare J.LThanE -> Lit . S.Boolean $ a <= b
            S.Compare J.GThan -> Lit . S.Boolean $ a > b
            S.Compare J.GThanE -> Lit . S.Boolean $ a >= b
            _ -> simplified
      (Lit (S.Boolean a), Lit (S.Boolean b)) ->
          case op of
            -- logic operations
            S.Logic J.And -> Lit . S.Boolean $ a && b
            S.Logic J.Or -> Lit . S.Boolean $ a || b
            _ -> simplified
      _ -> simplified
    where e1' = calc e1
          e2' = calc e2
          simplified = PrimOp e1' op e2'
calc (If e1 e2 e3) =
    case e1' of
      Lit (S.Boolean True) -> calc e2
      Lit (S.Boolean False) -> calc e3
      _ -> If e1' (calc e2) (calc e3)
    where e1' = calc e1
calc e = mapExpr calc e

peval :: Expr t (Expr t e) -> Expr t e
peval = calc . joinExpr . betaReduct
