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
                                Nothing -> error "transTyp: variable lookup failed!"
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
                                Nothing -> error "transExpr: variable lookup failed!"
    go (Lit (Integer i))   = FLit i
    go (BLam a expr)       = FBLam (\t -> transExpr (Map.insert a t tenv, eenv) expr)
    go (Lam (x, typ) expr) = FLam (transTyp tenv typ) (\e -> transExpr (tenv, Map.insert x e eenv) expr)
    go (TApp expr typ)     = FTApp (go expr) (transTyp tenv typ)
    go (App e1 e2)         = FApp (go e1) (go e2)
    go (PrimOp op e1 e2)   = FPrimOp (go e1) op (go e2)
    go (If0 predicate ifBr elseBr) = FIf0 (go predicate) (go ifBr) (go elseBr)
    go (Tuple exprs)       = FTuple [go e | e <- exprs]
    go (Proj expr i)       = FProj i (go expr)
    go (Let NonRec localBinds expr) = error "transExpr"
    go (Let Rec localBinds expr)    = error "transExpr"
