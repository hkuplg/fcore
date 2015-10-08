{- |
Module      :  Inliner
Description :  A simple heuristic inliner for FCore
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Yanlin <huohuohuomumu@gmail.com>
Stability   :  experimental
Portability :  portable

This module implements a simple heuristic inlining process for FCore.
-}


module Inliner where

import OptiUtils
import Core

-- TODO: inline definitions inside module?
inliner :: Expr t (Expr t e) -> Expr t e
inliner (Var _ x) = x
inliner (Lam n t1 e1) = Lam n t1 (inliner . e1 . var)
inliner (App e1 e2) = App (inliner e1) (inliner e2)
inliner (BLam n t1) = BLam n (inliner . t1)
inliner (TApp e t) = TApp (inliner e) t
inliner (Lit s) = Lit s
inliner (If e1 e2 e3) = If (inliner e1) (inliner e2) (inliner e3)
inliner (PrimOp e1 o e2) = PrimOp (inliner e1) o (inliner e2)
inliner (Tuple es) = Tuple (map inliner es)
inliner (Proj i e) = Proj i (inliner e)
inliner (Fix n1 n2 f t1 t2) = Fix n1 n2 (\name n -> joinExpr $ f (joinExpr $ lam t1 (f (var name))) (var n)) t1 t2
inliner (LetRec n f es1 es2) =
  LetRec n f
         (map joinExpr . es1 . map joinExpr . es1 . map var)
         (joinExpr . es2  . map var)
inliner (JNew name es) = JNew name (map inliner es)
inliner (JMethod jc m es cn) =
  JMethod (fmap inliner jc) m (map inliner es) cn
inliner (JField jc fn cn) = JField (fmap inliner jc) fn cn
inliner (Seq es) = Seq (map inliner es)
inliner (Let n e body) = Let n (inliner e) (inliner . body . var)
inliner e = joinExpr e

-- current version cannot handle LetRec
recurNum :: Expr t Int -> Int
recurNum (Var _ x) = x
recurNum (Lit _) = 0
recurNum (Lam _ _ f) = recurNum $ f 0
recurNum (Fix _ _ f _ _) = recurNum $ f 1 0
recurNum (Let _ e f) = recurNum e + recurNum (f 0)
recurNum (LetRec _ _ f1 f2) = sum (map recurNum (f1 (repeat 0))) + recurNum (f2 (repeat 0))
recurNum (App e1 e2) = recurNum e1 + recurNum e2
recurNum (If e1 e2 e3) = recurNum e1 + recurNum e2 + recurNum e3
recurNum (PrimOp e1 _ e2) = recurNum e1 + recurNum e2
recurNum _ = 0
