-- | Simple inliner

module Inliner where

import OptiUtils
import Core

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
inliner e@(Fix f t1 _) = Lam t1 (\n -> joinExpr $ f (joinExpr e) (Var n))
inliner (LetRec f es1 es2) =
  LetRec f
         (\es -> map joinExpr . es1 . map joinExpr . es1 $ map Var es)
         (\es -> joinExpr . es2 $ map Var es)
inliner (JNewObj name es) = JNewObj name (map inliner es)
inliner (JMethod jc m es cn) =
  JMethod (fmap inliner jc) m (map inliner es) cn
inliner (JField jc fn cn) = JField (fmap inliner jc) fn cn
inliner (Seq es) = Seq (map inliner es)
inliner (Merge e1 e2) = Merge (inliner e1) (inliner e2)
