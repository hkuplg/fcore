-- | Simple inliner

module Inliner where

import OptiUtils
import Core

inliner :: Expr t (Expr t e) -> Expr t e
inliner (Var _ x) = x
inliner (Lam n t1 e1) = Lam n t1 (\ee -> inliner (e1 (var ee)))
inliner (App e1 e2) = App (inliner e1) (inliner e2)
inliner (BLam n t1) = BLam n (\t2 -> inliner (t1 t2))
inliner (TApp e t) = TApp (inliner e) t
inliner (Lit s) = Lit s
inliner (If e1 e2 e3) = If (inliner e1) (inliner e2) (inliner e3)
inliner (PrimOp e1 o e2) = PrimOp (inliner e1) o (inliner e2)
inliner (Tuple es) = Tuple (map inliner es)
inliner (Proj i e) = Proj i (inliner e)
inliner (Fix n1 n2 f t1 t2) = Fix n1 n2 (\name n -> joinExpr $ f (joinExpr $ lam t1 (f (var name))) (var n)) t1 t2
inliner (LetRec f es1 es2) =
  LetRec f
         (\es -> map joinExpr . es1 . map joinExpr . es1 $ map var es)
         (\es -> joinExpr . es2 $ map var es)
inliner (JNew name es) = JNew name (map inliner es)
inliner (JMethod jc m es cn) =
  JMethod (fmap inliner jc) m (map inliner es) cn
inliner (JField jc fn cn) = JField (fmap inliner jc) fn cn
inliner (Seq es) = Seq (map inliner es)
inliner (Merge e1 e2) = Merge (inliner e1) (inliner e2)
inliner (Let n e body) = Let n (inliner e) (\x -> inliner (body (var x)))
