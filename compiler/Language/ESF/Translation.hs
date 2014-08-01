{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ESF.Translation
  ( transESF
  ) where

import Data.Maybe       (fromMaybe)
import Data.List        (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Java.Syntax as J (Op(..))

import Language.ESF.Syntax
import Language.ESF.TypeCheck
import Language.SystemF.Syntax

transESF :: Term -> PFExp t e
transESF e = case infer e of
               Nothing -> error "Typecheck failed"
               Just _  -> (transNfExp . transTerm) e

invariantFailed :: String -> String -> a
invariantFailed location msg = error ("Invariant failed in " ++ location ++ ": " ++ msg)

transType :: Map.Map Name t -> Type -> PFTyp t
transType d = go
  where
    go  Int         = FInt
    go (Fun t1 t2)  = FFun (go t1) (go t2)
    go (Product ts) = FProduct (map go ts)
    go (TVar a)     = case Map.lookup a d of
                        Just a' -> FTVar a'
                        Nothing -> invariantFailed "transType" ("Lookup failed for type variable " ++ a)
    go (Forall a t) = FForall (\a' -> transType (Map.insert a a' d) t)

-- System F (normal representation) closures
data NfExp =
      NfVar Name
    | NfBLam Name NfExp
    | NfLam (Name, Type) NfExp
    | NfTApp NfExp Type
    | NfApp NfExp NfExp
    | NfPrimOp NfExp J.Op NfExp
    | NfLit PrimLit
    | NfIf0 NfExp NfExp NfExp
    | NfTuple [NfExp]
    | NfProj Int NfExp
    -- fix x (x1 : t1) : t2. e
    | NfFix Name (Name, Type) Type NfExp

transNfExp :: NfExp -> PFExp t e
transNfExp = transNfExpWith (Map.empty, Map.empty)

transNfExpWith :: (Map.Map Name t, Map.Map Name e) -> NfExp -> PFExp t e
transNfExpWith (d, g) = go
  where
    go (NfVar x)              =
      case Map.lookup x g of
        Just e  -> FVar x e
        Nothing -> invariantFailed "transNfExpWith" ("Lookup failed for variable " ++ x)
    go (NfLit i)              = FLit i
    go (NfApp e1 e2)          = FApp (go e1) (go e2)
    go (NfPrimOp e1 op e2)    = FPrimOp (go e1) op (go e2)
    go (NfIf0 p i e)          = FIf0 (go p) (go i) (go e)
    go (NfTuple es)           = FTuple (map go es)
    go (NfProj i e)           = FProj i (go e)
    go (NfBLam a e)           = FBLam (\a' -> transNfExpWith (Map.insert a a' d, g) e)
    go (NfLam (x,t) e)        = FLam (transType d t)
                                    (\x' -> transNfExpWith (d, Map.insert x x' g) e)
    go (NfTApp e t)           = FTApp (go e) (transType d t)
    go (NfFix x (x1,t1) t2 e) =
      FFix (\x' x1' -> transNfExpWith (d, (Map.insert x1 x1' . Map.insert x x') g) e)
           (transType d t1) (transType d t2)

transTerm :: Term -> NfExp
transTerm = transTermWith (Set.empty, Map.empty)

transTermWith :: (TypeContext, ValueContext) -> Term -> NfExp
transTermWith (d, g) = go
  where
    go (Var x)             = NfVar x
    go (Lit (Integer n))   = NfLit n
    go (Lam (x, t) e)      = NfLam (x, t) (go e)
    go (App e1 e2)         = NfApp (go e1) (go e2)
    go (BLam a e)          = NfBLam a (go e)
    go (TApp e t)          = NfTApp (go e) t
    go (Tuple ts)          = NfTuple (map go ts)
    go (Proj e i)          = NfProj i (go e)
    go (PrimOp e1 op e2)   = NfPrimOp (go e1) op (go e2)
    go (If0 e1 e2 e3)      = NfIf0 (go e1) (go e2) (go e3)
    go (Let _ [] e)        = go e

    {-  Translation rule:
        let f1 = e1 in e
        ~> (\(f1 : infer e1). e) e1     -}
    go (Let NonRec [b] e) =
      case dsBind b of
        Nothing -> invariantFailed "transTermWith" ("dsBind failed for " ++ show b)
        Just (f1, e1, _) -> -- TODO: make use of the annotation
          case inferWith (d,g) e1 of
            Nothing -> invariantFailed "transTermWith"
                         ("Type inference failed for " ++ show e1)
            Just t1 -> NfApp (NfLam (f1, t1) (go e)) (go e1)

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
    go (Let NonRec bs@(_:_:_) e) =
      NfLam (f', t') (substMulti ss $ go e) `NfApp` go e'
        where
          f' = intercalate "_" fs -- TODO: make sure f' is fresh
          e' = Tuple es
          t' = fromMaybe
                 (invariantFailed "transTermWith" ("Failed to typecheck " ++ show e'))
                 (inferWith (d,g) e')
          ss  = zipWith (\f i -> (f, NfProj i (NfVar f'))) fs [0..length fs - 1]
          (fs, es, _) = unzip3 $
            (flip map) bs (\b ->
              case dsBind b of
                Nothing -> invariantFailed "transTermWith"
                             ("dsBind failed for " ++ show b)
                Just b' -> b')

    {- Translation rule:
        let rec f1 = e1, ..., fn = en in e
        ~> let f' = fix f' (dummy:Int) : (t1, ..., tn). (e1[...], ..., en[...]) in e[...]
           (let f' = e' in e)
        ~> (\(f' : Int -> (t1, ..., tn)). e[...]) e'[...]       -}
    -- TODO: note that currently it's possible that the tuple contains only one item
    -- TODO: really semantics-preserving with call-by-value?
    -- TODO: optimize for recursive let's with a single binding
    go (Let Rec bs@(_:_) e) =
      (NfLam (f', Int `Fun` Product ts) (substMulti ss $ go e)) `NfApp` substMulti ss e'
        where
          f' = intercalate "_" (map bindId bs) -- TODO: make sure f' is fresh
          e' = NfFix f' ("dummy", Int) (Product ts) (NfTuple $ map (substMulti ss . go) es)
          ts = (flip map) bs (\Bind{..} ->
                 case bindRhsAnnot of
                   Nothing     -> invariantFailed "transTerm"
                                    "Missing type signature for right hand side"
                   Just rhsTyp ->
                     wrap Forall bindTargs $ wrap Fun [t' | (_,t') <- bindArgs] rhsTyp)
          -- fi -> (f' 0)._(i-1)
          ss  = zipWith (\f i -> (f, NfProj i (NfVar f' `NfApp` NfLit 0)))
                  fs [0..length fs - 1]
          (fs, es, _) = unzip3 $
            (flip map) bs (\b ->
              case dsBind b of
                Nothing -> invariantFailed "transTermWith"
                             ("dsBind failed for " ++ show b)
                Just b' -> b')

freeVars :: NfExp -> Set.Set Name
freeVars (NfVar x)              = Set.singleton x
freeVars (NfBLam _ e)           = freeVars e
freeVars (NfLam (x,_) e)        = Set.delete x (freeVars e)
freeVars (NfTApp e t)           = freeVars e
freeVars (NfApp e1 e2)          = freeVars e1 `Set.union` freeVars e2
freeVars (NfPrimOp e1 _ e2)     = freeVars e1 `Set.union` freeVars e2
freeVars (NfLit _)              = Set.empty
freeVars (NfIf0 e1 e2 e3)       = freeVars e1 `Set.union` freeVars e2 `Set.union` freeVars e3
freeVars (NfTuple es)           = Set.unions (map freeVars es)
freeVars (NfProj i e)           = freeVars e
freeVars (NfFix x (x1,t1) t2 e) = (Set.delete x . Set.delete x1) (freeVars e)

class Subst a where
  subst :: (Name, a) -> a -> a
  substMulti :: [(Name, a)] -> a -> a
  substMulti ss x = foldl (\acc s -> subst s acc) x ss

instance Subst NfExp where
  subst = substFreeVars

-- Capture-avoiding substitution
-- http://en.wikipedia.org/wiki/Lambda_calculus#Capture-avoiding_substitutions
substFreeVars :: (Name, NfExp) -> NfExp -> NfExp
substFreeVars (x, r) = go
  where
    go (NfVar a)
      | a == x    = r
      | otherwise = NfVar a
    go (NfLam (y,t) e)
      | y == x                    = NfLam (y,t) e
      | y `Set.member` freeVars r = NfLam (y,t) e -- The freshness condition, crucial!
      | otherwise                 = NfLam (y,t) (go e)

    -- TODO: verify
    go (NfFix y (y1, t1) t2 e)
      | y == x                     = NfFix y (y1, t1) t2 e
      | y1 == x                    = NfFix y (y1, t1) t2 e
      | y  `Set.member` freeVars r = NfFix y (y1, t1) t2 e
      | y1 `Set.member` freeVars r = NfFix y (y1, t1) t2 e
      | otherwise                  = NfFix y (y1, t1) t2 (go e)

    go (NfBLam a e)        = NfBLam a (go e)
    go (NfTApp e t)        = NfTApp (go e) t
    go (NfApp e1 e2)       = NfApp (go e1) (go e2)
    go (NfPrimOp e1 op e2) = NfPrimOp (go e1) op (go e2)
    go (NfLit n)           = NfLit n
    go (NfTuple es)        = NfTuple (map go es)
    go (NfProj i e)        = NfProj i (go e)
    go (NfIf0 e1 e2 e3)    = NfIf0 (go e1) (go e2) (go e3)
