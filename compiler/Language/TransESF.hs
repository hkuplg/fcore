{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE RecordWildCards #-}

module Language.TransESF (transESF) where

import Language.ESF.Syntax
import Language.SystemF.Syntax

import Data.List (intercalate)
import qualified Data.Map as Map

-- Translate a *typechecked* EFS expression into System F
transESF :: Expr -> PFExp t e
transESF = transExpr (Map.empty, Map.empty, Map.empty)

type Mapping a = Map.Map String a
type Env       = Map.Map String Typ

transTyp :: Mapping t -> Typ -> PFTyp t
transTyp tmap = go
  where
    go (TVar a) =
      case Map.lookup a tmap of
        Just t  -> FTVar t
        Nothing -> error "transTyp: Type variable lookup failed"
    go Int                 = FInt
    go (Forall [] typ)     = go typ
    go (Forall (a:as) typ) = FForall (\t0 -> transTyp (Map.insert a t0 tmap) (Forall as typ))
    go (Fun t1 t2)         = FFun (go t1) (go t2)
    go (Product typs)      = FProduct [go t | t <- typs]

transExpr :: (Env, Mapping t, Mapping e) -> Expr -> PFExp t e
transExpr (env, tmap, emap) = go
  where
    go (Var x) =
      case Map.lookup x emap of
        Just e  -> FVar x e
        Nothing -> error "transExpr: Variable lookup failed"
    go (Lit (Integer i))   = FLit i
    go (BLam [] expr)      = go expr
    go (Lam [] expr)       = go expr
    go (BLam (a:as) expr)  = FBLam (\t0 -> transExpr (env, Map.insert a t0 tmap, emap) (BLam as expr))
    go (Lam (p:ps) expr)   = FLam (transTyp tmap t) (\e0 -> transExpr (env, tmap, Map.insert x e0 emap) (Lam ps expr))
      where (VarPat x, t)  = p
    go (TApp e t)          = FTApp (go e) (transTyp tmap t)
    go (App e1 e2)         = FApp (go e1) (go e2)
    go (PrimOp op e1 e2)   = FPrimOp (go e1) op (go e2)
    go (If0 p ifBr elseBr) = FIf0 (go p) (go ifBr) (go elseBr)
    go (Tuple exprs)       = FTuple [go e | e <- exprs]
    go (Proj expr i)       = FProj i (go expr)

    --     let rec f1 (x : t) : t' = e1 in e
    -- ~~> let f1 = fix (f1 : t -> t'). \x. e1 in e
    -- ~~> (\(f1 : t -> t'). e) (fix (f1 : t -> t'). \x. e1)
    go (Let Rec [lbind] e) = FApp lam fix
      where
        (f1, Lam [(VarPat x, t)] e1, Fun _ t') = dsLocalBind Rec env lbind
        lam = go (Lam [(VarPat f1, Fun t t')] e)
        fix = FFix (\f1_0 -> \x0 -> transExpr (env, tmap, (Map.insert x x0 . Map.insert f1 f1_0) emap) e1)
                   (transTyp tmap t) (transTyp tmap t')

    go e@(Let _ _ _)      = go (dsLet env e)

-- Eliminate all non-recursive let expressions, and rewrite mutually recursive
-- let expressions into non-mutually recursive ones
dsLet :: Env -> Expr -> Expr

--     let f1 = e1, f2 = e2, f3 = e3 in e
-- ~~> (\(f1 : infer e1). (\(f2 : infer e2). (\(f3 : infer e3). e) e3) e2) e1
dsLet env (Let NonRec [] expr)             = expr
dsLet env (Let NonRec (lbind:lbinds) expr) = App (Lam [(VarPat f1, t1)] inside) e1
  where (f1, e1, t1) = dsLocalBind NonRec env lbind
        inside       = dsLet env (Let NonRec lbinds expr)

--     let rec f1 = e1, f2 = e2, f3 = e3 in e
-- ~~> let rec f (dummy : Int) = (e1, e2, e3) in e
--     [ f1 -> (f 0)._0, f2 -> (f 0)._1, f3 = (f 0)._2 ] inside (e1, e2, e3) and e
dsLet env (Let Rec [] expr)      = expr
dsLet env (Let Rec [lbind] expr) = expr
dsLet env (Let Rec lbinds expr)  = Let Rec [lbind'] expr
  where
    (fs, es, ts) = unzip3 (map (dsLocalBind Rec env) lbinds)
    lbind' = LocalBind { local_id     = intercalate "_" fs -- TODO: Make sure the id is fresh
                       , local_targs  = []
                       , local_args   = [(VarPat "dummy", Int)] -- TODO: Make sure the id is free
                       , local_rettyp = Just (Product ts)
                       , local_rhs    = Tuple es
                       }

-- data LocalBind = LocalBind
--   { local_id     :: String       -- Identifier
--   , local_targs  :: [String]     -- Type arguments
--   , local_args   :: [(Pat, Typ)] -- Arguments, each annotated with a type
--   , local_rettyp :: Maybe Typ    -- Return type
--   , local_rhs    :: Expr         -- RHS to the "="
--   } deriving (Eq, Show)

dsLet _ expr = expr

--     f A1 ... An (x1 : T1) ... (xn : Tn) : T = e
-- ~~> ( f                                             -- name
--     , /\A1. ... /\An. \(x1 : T1). ... \(xn : Tn). e -- value
--     , forall A1 ... An. T1 -> ... -> Tn -> T        -- typ
--     )
dsLocalBind :: RecFlag -> Env -> LocalBind -> (String, Expr, Typ)
dsLocalBind recFlag env LocalBind{..} = (name, value, typ)
  where
    name  = local_id
    value = wrapExpr BLam local_targs (wrapExpr Lam local_args local_rhs)
    typ   = case (recFlag, local_rettyp) of
              (NonRec, _)        -> infer env value -- TODO: At the moment the annotation for return type is just discarded.
              (Rec, Just rettyp) -> wrapTyp Forall local_targs (joinTyps ([t | (_, t) <- local_args] ++ [rettyp]))
              (Rec, Nothing)     -> error "Type annotation required for recursive definitions"

infer :: Env -> Expr -> Typ
infer env = go
  where
    go (Lit (Integer _))     = Int
    go (BLam as expr)        = Forall as (go expr)
    go (Lam [] expr)         = go expr
    go (Lam ((VarPat x,t):xs) expr) = Fun t (infer (Map.insert x t env) (Lam xs expr))
    go (Tuple exprs)         = Product [go e | e <- exprs]
    go (Var x) =
      case Map.lookup x env of
        Just t  -> t
        Nothing -> error ("infer: Unbound variable `" ++ x ++ "'")
    go (TApp e _) =
      case go e of
        Forall _ t -> t
        _          -> error "infer: Expect the first argument of TApp to have type Forall"
    go (App e1 e2) =
      case go e1 of
        Fun argtyp rettyp | go e2 == argtyp -> rettyp
        Fun argtyp _ -> error ("infer: Couldn't match expected argument type `" ++ show argtyp
                              ++ "' with actual type `" ++ show (go e2) ++ "'")
        _            -> error "infer: Expect the first argument App to have type Fun"
    go (Proj e i) =
      case go e of
        Product ts | i < length ts -> ts !! i
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
    go (Let recFlag lbinds expr) = infer env' expr
      where
        env' = updateEnv [(f,t) | (f,_e,t) <- map (dsLocalBind recFlag env) lbinds] env
          where
            updateEnv [] env0              = env0
            updateEnv ((f1, t1):rest) env0 = updateEnv rest (Map.insert f1 t1 env0)

-- Utilities

-- Take a list of Typ's and return T1 -> T2 -> ... -> Tn
joinTyps :: [Typ] -> Typ
joinTyps []     = error "joinTyps: Empty list"
joinTyps [t]    = t
joinTyps (t:ts) = Fun t (joinTyps ts)

wrapTyp :: ([a] -> Typ -> Typ) -> [a] -> Typ -> Typ
wrapTyp cons []     expr = expr
wrapTyp cons (x:xs) expr = cons [x] (wrapTyp cons xs expr)

wrapExpr :: ([a] -> Expr -> Expr) -> [a] -> Expr -> Expr
wrapExpr cons []     expr = expr
wrapExpr cons (x:xs) expr = cons [x] (wrapExpr cons xs expr)
