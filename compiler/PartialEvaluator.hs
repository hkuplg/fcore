module PartialEvaluator where

import Core
import OptiUtils
import qualified Src as S
import qualified Language.Java.Syntax as J (Op(..))

peval :: Expr t (Expr t e) -> Expr t e
peval (Var x) = x
peval (Lam t f) = Lam t (\x -> peval . f . Var $ x)
peval (BLam f) = BLam (\t -> peval . f $ t)
peval (App e1 e2) =
    case e1 of
      Lam _ f -> peval . f . peval $ e2
      _ -> App (peval e1) (peval e2)
{-
    -- type-error. f2 is of type "e -> Expr t e" while f is of type "Expr t e -> Expr t (Expr t e)"
    case peval e1 of
      Lam t f2 -> f2 $ peval e2
-}
peval (TApp e t) = TApp (peval e) t
peval (Lit s) = Lit s
peval (If e1 e2 e3) =
    case e1' of
      Lit (S.Boolean True) -> peval e2
      Lit (S.Boolean False) -> peval e3
      _ -> If e1' (peval e2) (peval e3)
    where e1' = peval e1
peval (PrimOp e1 op e2) = -- TODO: op excludes (LShift RShift RRShift Xor CAnd COr)
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
    where e1' = peval e1
          e2' = peval e2
          simplified = PrimOp e1' op e2'
peval (Tuple es) = Tuple $ map peval es
peval (Proj i e) = Proj i (peval e)
peval (Fix f t1 t) = Fix (\e1 e2 -> peval $ f (Var e1) (Var e2)) t1 t
peval (LetRec sigs binds body) =
    LetRec sigs
           (\es -> map peval . binds $ map Var es)
           (\es -> peval . body $ map Var es)
peval (JNewObj cname es) = JNewObj cname (map peval es)
peval (JMethod cnameOrE mname es cname) = JMethod (fmap peval cnameOrE) mname (map peval es) cname
peval (JField cnameOrE fname cname) = JField (fmap peval cnameOrE) fname cname
peval (Seq es) = Seq $ map peval es
peval (Merge e1 e2) = Merge (peval e1) (peval e2)
