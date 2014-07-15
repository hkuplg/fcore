{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE RecordWildCards #-}

module Language.TransESF (transESF) where

import Language.ESF.Syntax
import Language.SystemF.Syntax

import qualified Data.Map as Map

-- Translate an EFS expression into System F
transESF :: Expr -> PFExp t e
transESF = transExpr (Map.empty, Map.empty)

transTyp :: Map.Map String t -> Typ -> PFTyp t
transTyp tenv = go
  where
    go (TVar a)            = case Map.lookup a tenv of
                               Just t  -> FTVar t
                               Nothing -> error "transTyp: variable lookup failed"
    go Int                 = FInt
    go (Forall [] typ)     = go typ
    go (Forall (a:as) typ) = FForall (\t -> transTyp (Map.insert a t tenv) (Forall as typ))
    go (Fun t1 t2)         = FFun (go t1) (go t2)
    go (Product typs)      = FProduct [go t | t <- typs]

transExpr :: (Map.Map String t, Map.Map String e) -> Expr -> PFExp t e
transExpr (tenv, eenv) = go
  where
    go (Var x)             = case Map.lookup x eenv of
                               Just e  -> FVar x e
                               Nothing -> error "transExpr: variable lookup failed"
    go (Lit (Integer i))   = FLit i
    go (BLam a expr)       = FBLam (\t -> transExpr (Map.insert a t tenv, eenv) expr)
    go (Lam (x, typ) expr) = FLam (transTyp tenv typ) (\e -> transExpr (tenv, Map.insert x e eenv) expr)
    go (TApp expr typ)     = FTApp (go expr) (transTyp tenv typ)
    go (App e1 e2)         = FApp (go e1) (go e2)
    go (PrimOp op e1 e2)   = FPrimOp (go e1) op (go e2)
    go (If0 p ifBr elseBr) = FIf0 (go p) (go ifBr) (go elseBr)
    go (Tuple exprs)       = FTuple [go e | e <- exprs]
    go (Proj expr i)       = FProj i (go expr)
    go expr@(Let _ _ _)    = go (dsLet Map.empty expr) -- TODO: Should this really be an empty map?

infer :: Map.Map String Typ -> Expr -> Typ
infer env = go
  where
    go (Var x)             = case Map.lookup x env of
                               Just typ -> typ
                               Nothing  -> error ("infer: Unbound variable `" ++ x ++ "'")

    go (Lit (Integer _))   = Int
    go (BLam a expr)       = Forall [a] (go expr)
    go (Lam (_, typ) expr) = Fun typ (go expr)

    go (TApp expr _)       = case go expr of
                               Forall _ typ -> typ
                               _            -> error "infer: Expect the first argument of TApp to have type Forall"

    go (App e1 e2)         = case go e1 of
                               Fun argtyp rettyp | go e2 == argtyp -> rettyp
                               Fun argtyp _ -> error ("infer: Couldn't match expected argument type `" ++ show argtyp
                                                      ++ "' with actual type `" ++ show (go e2) ++ "'")
                               _            -> error "infer: Expect the first argument App to have type Fun"

    go (Tuple exprs)       = Product [go e | e <- exprs]

    go (Proj expr i)       = case go expr of
                               Product typs | i < length typs -> typs !! i
                               Product _ -> error "infer: Index out of bound for Proj"
                               _         -> error "infer: Expect the first argument of Proj to have type Product"

    go (PrimOp _ e1 e2)
      | (t1 == t2 && t1 == Int) = Int
      | t1 /= Int               = error "infer: Expect the first argument of PrimOp to have type Int"
      | otherwise               = error "infer: Expect the second argument of PrimOp to have type Int"
      where t1 = go e1
            t2 = go e2

    go (If0 p ifBr elseBr)
      | (go p == Int && go ifBr == go elseBr) = go ifBr
      | go p /= Int = error "infer: Expect the predicate of an If expression to have type Int"
      | otherwise   = error "infer: Type mismatch in two branches of an If expression"

    go expr@(Let _ _ _) = error "infer: Let case not implemented"

dsLet :: Map.Map String Typ -> Expr -> Expr
dsLet env (Let NonRec localBinds expr) = App (Lam (undefined, infer env groupedDefs) expr) groupedDefs
  where groupedDefs = Tuple [let (_, def) = dsPat bind in def | bind <- localBinds]
dsLet env (Let Rec localBinds expr) = undefined
dsLet _ expr = expr

dsPat :: LocalBind -> (String, Expr)
dsPat LocalBind{..} = (local_id, wrapBLam local_targs (wrapLam local_args local_rhs))

wrapBLam :: [String] -> Expr -> Expr
wrapBLam []     expr = expr
wrapBLam (a:as) expr = BLam a (wrapBLam as expr)

wrapLam :: [(String, Typ)] -> Expr -> Expr
wrapLam []     expr = expr
wrapLam (x:xs) expr = Lam x (wrapLam xs expr)
