module PartialEvaluator (peval) where

import Core
import OptiUtils
import qualified Src as S
import qualified Language.Java.Syntax as J (Op(..))

betaReduct :: Expr t (Expr t e) -> Expr t (Expr t e)
betaReduct (Lam t f) = Lam t (\x -> betaReduct . f $ x)
betaReduct (BLam f) = BLam (\t -> betaReduct . f $ t)
betaReduct (App e1 e2) =
    case betaReduct e1 of
      Lam _ f -> f . joinExpr . betaReduct $ e2
      _ -> App (betaReduct e1) (betaReduct e2)
betaReduct (If e1 e2 e3) = If (betaReduct e1) (betaReduct e2) (betaReduct e3)
betaReduct (TApp e t) = TApp (betaReduct e) t
betaReduct (PrimOp e1 op e2) = PrimOp (betaReduct e1) op (betaReduct e2)
betaReduct (Tuple es) = Tuple $ map betaReduct es
betaReduct (Proj i e) = Proj i (betaReduct e)
betaReduct (Fix f t1 t) = Fix (\e1 e2 -> betaReduct $ f e1 e2) t1 t
betaReduct (LetRec sigs binds body) =
    LetRec sigs
           (\es -> map betaReduct $ binds es)
           (\es -> betaReduct $ body es)
betaReduct (JNewObj cname es) = JNewObj cname (map betaReduct es)
betaReduct (JMethod cnameOrE mname es cname) = JMethod (fmap betaReduct cnameOrE) mname (map betaReduct es) cname
betaReduct (JField cnameOrE fname cname) = JField (fmap betaReduct cnameOrE) fname cname
betaReduct (Seq es) = Seq $ map betaReduct es
betaReduct (Merge e1 e2) = Merge (betaReduct e1) (betaReduct e2)
betaReduct e = e

calc :: Expr t e -> Expr t e
calc (If e1 e2 e3) =
    case calc e1 of
      Lit (S.Boolean True) -> calc e2
      Lit (S.Boolean False) -> calc e3
      _ -> If (calc e1) (calc e2) (calc e3)
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
calc e = e

peval :: Expr t (Expr t e) -> Expr t e
peval = calc . joinExpr . betaReduct
