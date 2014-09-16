{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple inliner

module Inliner where

import Core

inliner :: Expr t (Expr t e) -> Expr t e
inliner (Var x) = x
inliner (Lam t1 e1) = undefined
inliner (BLam t1) = BLam (\t2 -> inliner (t1 t2))
inliner (TApp e t) = TApp (inliner e) t
inliner (Lit s) = Lit s
inliner (If e1 e2 e3) = If (inliner e1) (inliner e2) (inliner e3)
inliner (PrimOp e1 o e2) = PrimOp (inliner e1) o (inliner e2)
inliner (Tuple es) = Tuple (map inliner es)
inliner (Proj i e) = Proj i (inliner e)
inliner (Fix f t1 t2) = undefined
-- inliner (LetRec s b1 b2) = \es -> inliner (b2 (map inliner (b1 es)))
inliner (LetRec s b1 b2) = undefined
inline (App e1 e2) = App (inliner e1) (inliner e2)
