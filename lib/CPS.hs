{- |
Module      :  BaseTransCFJava
Description :  Basic translation of FCore to Java
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Unknown
Stability   :  stable
Portability :  non-portable (MPTC)

This module implements the basic translation of FCore to Java. For
more information, please refer to the paper on wiki.
-}


module CPS where

import Core
import Panic

cpsTrans :: Expr t (Expr t e) -> Expr t e -- TODO: is the type signature correct?
cpsTrans (Var _ x) = x
cpsTrans (Lam n t1 e1) =
  Lam n
      t1
      (\x ->
         Lam n
             (Fun t1 t1)
             (\k ->
                App (var k)
                    (cpsTrans (e1 (var x)))))
cpsTrans (App e1 e2) = App (cpsTrans e1) (cpsTrans e2)
cpsTrans (BLam n t1) = BLam n (cpsTrans . t1)
cpsTrans (TApp e t) = TApp (cpsTrans e) t
cpsTrans (Lit s) = Lit s
cpsTrans (If e1 e2 e3) = If (cpsTrans e1) (cpsTrans e2) (cpsTrans e3)
cpsTrans (PrimOp e1 o e2) = PrimOp (cpsTrans e1) o (cpsTrans e2)
-- cpsTrans (Tuple es) = Tuple (map cpsTrans es)
-- cpsTrans (PolyList es t) = PolyList (map cpsTrans es) t
-- cpsTrans (JProxyCall jmethod t) = JProxyCall (cpsTrans jmethod) t
-- cpsTrans (Data name params ctrs e) = Data name params ctrs (cpsTrans e)
-- cpsTrans (Constr ctr es) = Constr ctr (map cpsTrans es)
-- cpsTrans (Case e alts) = Case (cpsTrans e) (map joinAlt alts)
--   where joinAlt (ConstrAlt ctr vars f) = ConstrAlt ctr vars (cpsTrans . f . zipWith Var vars)
-- cpsTrans (Proj i e) = Proj i (cpsTrans e)
-- cpsTrans (Fix n1 n2 f t1 t2) = Fix n1 n2 (\e1 e2 -> cpsTrans (f (Var n1 e1) (Var n2 e2))) t1 t2
-- cpsTrans (Let n bind body) = Let n (cpsTrans bind) (cpsTrans . body . Var n)
-- cpsTrans (LetRec n s b1 b2) =
--   LetRec n s
--          (map cpsTrans . b1 . zipWith Var n)
--          (cpsTrans . b2 . zipWith Var n)
-- cpsTrans (JNew name es) = JNew name (map cpsTrans es)
-- cpsTrans (JMethod jc m es cn) =
--   JMethod (fmap cpsTrans jc) m (map cpsTrans es) cn
-- cpsTrans (JField jc fn cn) = JField (fmap cpsTrans jc) fn cn
-- cpsTrans (Seq es) = Seq (map cpsTrans es)
-- cpsTrans _ = sorry "Not implemented yet"
