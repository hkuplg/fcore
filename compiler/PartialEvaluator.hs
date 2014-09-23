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
calc (Lam t f) = Lam t (\x -> calc . f $ x)
calc (BLam f) = BLam (\t -> calc . f $ t)
calc (App e1 e2) = App (calc e1) (calc e2)
calc (TApp e t) = TApp (calc e) t
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
calc (Tuple es) = Tuple $ map calc es
calc (Proj i e) = Proj i (calc e)
calc (Fix f t1 t) = Fix (\e1 e2 -> calc $ f e1 e2) t1 t
calc (LetRec sigs binds body) =
    LetRec sigs
           (\es -> map calc $ binds es)
           (\es -> calc $ body es)
calc (JNewObj cname es) = JNewObj cname (map calc es)
calc (JMethod cnameOrE mname es cname) = JMethod (fmap calc cnameOrE) mname (map calc es) cname
calc (JField cnameOrE fname cname) = JField (fmap calc cnameOrE) fname cname
calc (Seq es) = Seq $ map calc es
calc (Merge e1 e2) = Merge (calc e1) (calc e2)
calc (If e1 e2 e3) =
    case calc e1 of
      Lit (S.Boolean True) -> calc e2
      Lit (S.Boolean False) -> calc e3
      _ -> If (calc e1) (calc e2) (calc e3)
calc e = e

mapExpr :: (Expr t e -> Expr t e) -> Expr t e -> Expr t e
mapExpr f e =
    case e of
      Var x -> e
      Lit l -> e
      Lam t g -> Lam t (\x -> f . g $ x)
      Fix g t1 t -> Fix (\e1 e2 -> f $ g e1 e2) t1 t
      LetRec sigs binds body ->
          LetRec sigs
                 (\es -> map f $ binds es)
                 (\es -> f $ body es)
      BLam g -> BLam (\x -> f . g $ x)
      App e1 e2 -> App (f e1) (f e2)
      TApp e t -> TApp (f e) t
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      PrimOp e1 op e2 -> PrimOp (f e1) op (f e2)
      Tuple es -> Tuple $ map f es
      Proj i e -> Proj i (f e)
      JNewObj cname es -> JNewObj cname (map f es)
      JMethod cnameOrE mname es cname -> JMethod (fmap f cnameOrE) mname (map f es) cname
      JField cnameOrE fname cname -> JField (fmap f cnameOrE) fname cname
      Seq es -> Seq $ map f es
      Merge e1 e2 -> Merge (f e1) (f e2)

peval :: Expr t (Expr t e) -> Expr t e
peval = calc . joinExpr . betaReduct
