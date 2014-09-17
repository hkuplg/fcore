-- | Simple inliner

module Inliner where

import Core
import Examples

inliner :: Expr t (Expr t e) -> Expr t e
inliner (Var x) = x
inliner (Lam t1 e1) = Lam t1 (\ee -> inliner (e1 (Var ee)))
inliner (App e1 e2) = App (inliner e1) (inliner e2)
inliner (BLam t1) = BLam (\t2 -> inliner (t1 t2))
inliner (TApp e t) = TApp (inliner e) t
inliner (Lit s) = Lit s
inliner (If e1 e2 e3) = If (inliner e1) (inliner e2) (inliner e3)
inliner (PrimOp e1 o e2) = PrimOp (inliner e1) o (inliner e2)
inliner (Tuple es) = Tuple (map inliner es)
inliner (Proj i e) = Proj i (inliner e)
inliner e@(Fix f _ t2) = Lam t2 (\n -> joinExpr (f (joinExpr e) (Var n)))
inliner (LetRec f es1 es2) = LetRec f
                                    (\es -> map joinExpr (es1 (map Var es)))
                                    (\es -> joinExpr (es2 (map joinExpr (es1 (map Var es)))))
inliner (JNewObj name es) = JNewObj name (map inliner es)
inliner (JMethod jc m es cn) =
  JMethod (fmap inliner jc) m (map inliner es) cn
inliner (JField jc fn cn) = JField (fmap inliner jc) fn cn
inliner (Seq es) = Seq (map inliner es)
inliner (Merge e1 e2) = Merge (inliner e1) (inliner e2)

joinExpr :: Expr t (Expr t e) -> Expr t e
joinExpr (Var x) = x
joinExpr (Lam t1 e1) = Lam t1 (\ee -> joinExpr (e1 (Var ee)))
joinExpr (App e1 e2) = App (joinExpr e1) (joinExpr e2)
joinExpr (BLam t1) = BLam (\t2 -> joinExpr (t1 t2))
joinExpr (TApp e t) = TApp (joinExpr e) t
joinExpr (Lit s) = Lit s
joinExpr (If e1 e2 e3) = If (joinExpr e1) (joinExpr e2) (joinExpr e3)
joinExpr (PrimOp e1 o e2) = PrimOp (joinExpr e1) o (joinExpr e2)
joinExpr (Tuple es) = Tuple (map joinExpr es)
joinExpr (Proj i e) = Proj i (joinExpr e)
joinExpr (Fix f t1 t2) = Fix (\e1 e2 -> joinExpr (f (Var e1) (Var e2))) t1 t2
joinExpr (LetRec s b1 b2) =
  LetRec s
          (\es -> map joinExpr (b1 (map Var es)))
          (\es -> joinExpr (b2 (map Var es)))
joinExpr (JNewObj name es) = JNewObj name (map joinExpr es)
joinExpr (JMethod jc m es cn) =
  JMethod (fmap joinExpr jc) m (map joinExpr es) cn
joinExpr (JField jc fn cn) = JField (fmap joinExpr jc) fn cn
joinExpr (Seq es) = Seq (map joinExpr es)
joinExpr (Merge e1 e2) = Merge (joinExpr e1) (joinExpr e2)

examp :: Expr t e
examp = inliner fact
