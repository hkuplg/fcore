{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ESF.Translation (transESF) where

import Language.ESF.Syntax
import Language.SystemF.Syntax

import Data.Maybe       (fromMaybe, fromJust)
import Data.List        (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

transType :: Map.Map Name t -> Type -> PFTyp t
transType tmap = go
  where
    go Int               = FInt
    go (Fun t1 t2)       = FFun (go t1) (go t2)
    go (Product ts)      = FProduct (map go ts)
    go (TVar a)          =
      case Map.lookup a tmap of
        Just a' -> FTVar a'
        Nothing -> error ("transType: Lookup failed for type variable " ++ a)
    go (Forall a t)      = FForall (\a' -> transType (Map.insert a a' tmap) t)

transTerm :: Map.Map Name Type -> (Map.Map Name t, Map.Map Name e) -> Term -> PFExp t e
transTerm env (tmap, emap) = go
  where
    go (Var x)            =
      case Map.lookup x emap of
        Just e  -> FVar x e
        Nothing -> error ("transTerm: Lookup failed for variable " ++ x)
    go (Lit (Integer i))  = FLit i
    go (App e1 e2)        = FApp (go e1) (go e2)
    go (PrimOp op e1 e2)  = FPrimOp (go e1) op (go e2)
    go (If0 p i e)        = FIf0 (go p) (go i) (go e)
    go (Tuple es)         = FTuple (map go es)
    go (Proj expr i)      = FProj i (go expr)
    go (BLam a e)         = FBLam (\a' -> transTerm env (Map.insert a a' tmap, emap) e)
    go (Lam (x,t) e)      = FLam (transType tmap t) (\x' -> transTerm env (tmap, Map.insert x x' emap) e)
    go (TApp e t)         = FTApp (go e) (transType tmap t)

    --     let rec f (x : t1) : t2 = def in body
    -- ~~> let f = fix (f : t1 -> t2). \x. def in body
    -- ~~> (\(f : t1 -> t2). body) (fix (f : t1 -> t2). \x. def)
    go (Let Rec [lbind] body) = FApp lam fix
      where
        (f, Lam [(x, t1)] def, Fun _should_equal_t1 t2) = dsLocalBind Rec env lbind
        lam = go (Lam [(f, Fun t1 t2)] body)
        fix = FFix (\f' x' -> transTerm env (tmap, (Map.insert x x' . Map.insert f f') emap) def) (transType tmap t1) (transType tmap t2)

    go e@(Let _ _ _)      = go (dsLet NonRec e)

-- Eliminate all non-recursive let expressions, and rewrite mutually recursive
-- let expressions into non-mutually recursive ones
dsLet :: Map.Map Name Type -> Term -> Term
dsLet = go
  where
    --     let f1 = e1, f2 = e2, f3 = e3 in e
    -- ~~> (\(f1 : infer e1). (\(f2 : infer e2). (\(f3 : infer e3). e) e3) e2) e1
    go env (Let NonRec [] body)             = body
    go env (Let NonRec (lbind:lbinds) body) = App (Lam [(f1, t1)] inside) e1
      where (f1, e1, t1) = dsLocalBind NonRec env lbind
            inside       = go env (Let NonRec lbinds body)

    --     let rec f1 = e1, f2 = e2, f3 = e3 in e
    -- ~~> let rec f (dummy : Int) : (...) = (e1, e2, e3) in e
    -- ~~> let rec f (dummy : Int) : (...) =
    --     [ f1 -> (f 0)._0, f2 -> (f 0)._1, f3 = (f 0)._2 ] inside (e1, e2, e3) and e
    go env (Let Rec [] body)      = body
    go env (Let Rec [lbind] body) = Let Rec [lbind] body
    go env (Let Rec lbinds body)  = Let Rec [lbind'] (Let NonRec [] body)
      where
        (fs, es, ts) = unzip3 (map (dsLocalBind Rec env) lbinds)
        mergedName   = intercalate "_" fs -- TODO Make sure the id is fresh
        lbind' = LocalBind { local_id     = mergedName
                           , local_targs  = []
                           , local_args   = [("dummy", Int)]    -- TODO Make sure the id is free
                           , local_rettyp = Just (Product ts)
                           , local_rhs    = Tuple es
                           }
    go _ expr = expr
