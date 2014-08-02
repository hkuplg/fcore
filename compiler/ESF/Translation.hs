{-# LANGUAGE RecordWildCards #-}

module ESF.Translation
  ( transTcESF
  ) where


import qualified Language.Java.Syntax as J (Op(..))

import ESF.Syntax
import ESF.TypeCheck

import SystemF.Syntax

import Data.Maybe       (fromJust, fromMaybe)
import Data.List        (intercalate)

import qualified Data.Map as Map
import qualified Data.Set as Set

-- Translate a typechecked ESF expression into System F
transTcESF :: Expr (String, Type) -> PFExp t e
transTcESF = transNFExp . transTcExpr

transType :: Map.Map String t -> Type -> PFTyp t
transType d = go
  where
    go (TyVar a)    = FTVar $ fromJust (Map.lookup a d)
    go  Int         = FInt
    go (Fun t1 t2)  = FFun (go t1) (go t2)
    go (Product ts) = FProduct (map go ts)
    go (Forall a t) = FForall (\a' -> transType (Map.insert a a' d) t)

-- System F (normal representation)
data NFExp =
      NVar String
    | NBLam String NFExp
    | NLam (String, Type) NFExp
    | NTApp NFExp Type
    | NApp NFExp NFExp
    | NPrimOp NFExp J.Op NFExp
    | NLit PrimLit
    | NIf0 NFExp NFExp NFExp
    | NTuple [NFExp]
    | NProj Int NFExp
    -- fix x (x1 : t1) : t2. e
    | NFix String (String, Type) Type NFExp

transNFExp :: NFExp -> PFExp t e
transNFExp = transNFExpWith (Map.empty, Map.empty)

transNFExpWith :: (Map.Map String t, Map.Map String e) -> NFExp -> PFExp t e
transNFExpWith (d, g) = go
  where
    -- TODO: use fromJust
    go (NVar x)              = FVar "" (fromMaybe
                                         (error $ "transNFExpWith: NVar" ++ x ++ show (map fst (Map.toList g)))
                                         (Map.lookup x g)) -- TODO: no name
    go (NLit n)              = FLit n
    go (NApp e1 e2)          = FApp (go e1) (go e2)
    go (NPrimOp e1 op e2)    = FPrimOp (go e1) op (go e2)
    go (NIf0 p i e)          = FIf0 (go p) (go i) (go e)
    go (NTuple es)           = FTuple (map go es)
    go (NProj i e)           = FProj i (go e)
    go (NBLam a e)           = FBLam (\a' -> transNFExpWith (Map.insert a a' d, g) e)
    go (NLam (x,t) e)        = FLam
                                  (transType d t)
                                  (\x' -> transNFExpWith (d, Map.insert x x' g) e)
    go (NTApp e t)           = FTApp (go e) (transType d t)
    go (NFix x (x1,t1) t2 e) =
      FFix
        (\x' x1' -> transNFExpWith (d, (Map.insert x1 x1' . Map.insert x x') g) e)
        (transType d t1)
        (transType d t2)

transTcExpr :: Expr (String, Type) -> NFExp
transTcExpr = go
  where
    go (Var (x,_t))        = NVar x
    go (Lit (Integer n))   = NLit n
    go (Lam (x, t) e)      = NLam (x, t) (go e)
    go (App e1 e2)         = NApp (go e1) (go e2)
    go (BLam a e)          = NBLam a (go e)
    go (TApp e t)          = NTApp (go e) t
    go (Tuple ts)          = NTuple (map go ts)
    go (Proj e i)          = NProj i (go e)
    go (PrimOp e1 op e2)   = NPrimOp (go e1) op (go e2)
    go (If0 e1 e2 e3)      = NIf0 (go e1) (go e2) (go e3)
    go (Let _ [] e)        = go e

    {-  Translation rule:
        let f1 = e1 in e
        ~> (\(f1 : infer e1). e) e1     -}
    -- go (Let NonRec [b] e) =
    --   case dsBind b of
    --     Nothing -> invariantFailed "transTcExpr" ("dsBind failed for " ++ show b)
    --     Just (f1, e1, _) ->
    --       case inferWith (d,g) e1 of
    --         Nothing -> invariantFailed "transTcExpr"
    --                      ("Type inference failed for " ++ show e1)
    --         Just t1 -> NApp (NLam (f1, t1) (go e)) (go e1)

    {-  Note that rewriting simultaneous let expressions by nesting is wrong.
        A counter-example:
        # let x = 1 in let x = 2 and y = x in y;;
        - : int = 1
        # let x = 1 in let x = 2 in let y = x in y;;
        - : int = 2

        Translation rule:
        let f1 = e1, ..., fn = en in e
        ~> let f' = (e1, ..., en) in e [f'._0, ..., f'._(n-1) / f1, ..., fn]
           (let f' = e' in e)
        ~> (\(f' : infer e'). e[...]) e'        -}
    -- go (Let NonRec bs@(_:_:_) e) =
    --   NLam (f', t') (substMulti ss $ go e) `NApp` go e'
    --     where
    --       f' = intercalate "_" fs -- TODO: make sure f' is fresh
    --       e' = Tuple es
    --       t' = fromMaybe
    --              (invariantFailed "transTcExpr" ("Failed to typecheck " ++ show e'))
    --              (inferWith (d,g) e')
    --       ss  = zipWith (\f i -> (f, NProj i (NVar f'))) fs [0..length fs - 1]
    --       (fs, es, _) = unzip3 $
    --         map
    --           (\b ->
    --             fromMaybe
    --               (invariantFailed "transTcExpr" ("dsBind failed for " ++ show b))
    --               (dsBind b)
    --           bs)

    {- Translation rule:
        let rec f1 = e1, ..., fn = en in e
        ~> let y = fix y (dummy:Int) : (t1, ..., tn). (e1[...], ..., en[...]) in e[...]
           (let y = fix in e)
        ~> (\(y : Int -> (t1, ..., tn)). e[...]) fix[...]       -}
    -- TODO: note that currently it's possible that the tuple contains only one item
    -- TODO: really semantics-preserving with call-by-value?
    -- TODO: optimize for recursive let's with a single binding
    go (Let recFlag bs@(_:_) e) =
      NLam (y, Int `Fun` Product ts) (substMulti ss $ go e) `NApp` fix
        where
          -- TODO: make sure y is free
          y = '_' : intercalate "_" [ f | (f,_t) <- map bindId bs ]
          fix = NFix
                  y ("_dummy", Int) (Product ts) -- TODO: make sure _dummy is free
                  (NTuple $ map (case recFlag of { Rec -> substMulti ss; NonRec -> id } . go) es)
          ts = [ t | (_,t) <- map bindId bs ]
          es = map
                (\Bind{..} -> wrap BLam bindTargs $
                                wrap Lam bindArgs
                                  bindRhs)
                bs
          -- Substitutes: fi -> (y 0)._(i-1)
          ss = zipWith
                (\f i -> (f, NProj i (NVar y `NApp` NLit 0)))
                [ f | (f,_t) <- map bindId bs ]
                [0..length bs - 1]

freeVars :: NFExp -> Set.Set String
freeVars (NVar x)            = Set.singleton x
freeVars (NBLam _a e)        = freeVars e
freeVars (NLam (x,_t) e)     = Set.delete x (freeVars e)
freeVars (NTApp e _t)        = freeVars e
freeVars (NApp e1 e2)        = freeVars e1 `Set.union` freeVars e2
freeVars (NPrimOp e1 _op e2) = freeVars e1 `Set.union` freeVars e2
freeVars (NLit _n)           = Set.empty
freeVars (NIf0 e1 e2 e3)     = freeVars e1 `Set.union`
                                freeVars e2 `Set.union`
                                freeVars e3
freeVars (NTuple es)         = Set.unions [ freeVars e | e <- es ]
freeVars (NProj _i e)        = freeVars e
freeVars (NFix x (x1,_t1) _t2 e) = (Set.delete x . Set.delete x1) (freeVars e)

substMulti :: [(String, NFExp)] -> NFExp -> NFExp
substMulti ss x = foldl (flip subst) x ss

-- Capture-avoiding substitution
-- http://en.wikipedia.org/wiki/Lambda_calculus#Capture-avoiding_substitutions
subst :: (String, NFExp) -> NFExp -> NFExp
subst (x, r) = go
  where
    go (NVar a)
      | a == x    = r
      | otherwise = NVar a
    go (NLam (y,t) e)
      | y == x                    = NLam (y,t) e
      | y `Set.member` freeVars r = NLam (y,t) e -- The freshness condition, crucial!
      | otherwise                 = NLam (y,t) (go e)

    -- TODO: verify
    go (NFix y (y1, t1) t2 e)
      | y == x                     = NFix y (y1, t1) t2 e
      | y1 == x                    = NFix y (y1, t1) t2 e
      | y  `Set.member` freeVars r = NFix y (y1, t1) t2 e
      | y1 `Set.member` freeVars r = NFix y (y1, t1) t2 e
      | otherwise                  = NFix y (y1, t1) t2 (go e)

    go (NBLam a e)        = NBLam a (go e)
    go (NTApp e t)        = NTApp (go e) t
    go (NApp e1 e2)       = NApp (go e1) (go e2)
    go (NPrimOp e1 op e2) = NPrimOp (go e1) op (go e2)
    go (NLit n)           = NLit n
    go (NTuple es)        = NTuple (map go es)
    go (NProj i e)        = NProj i (go e)
    go (NIf0 e1 e2 e3)    = NIf0 (go e1) (go e2) (go e3)
