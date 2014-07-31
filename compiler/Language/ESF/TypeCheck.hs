{-# LANGUAGE RecordWildCards #-}
module Language.ESF.TypeCheck where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.ESF.Syntax

-- https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf

wellformed :: TypeContext -> Type -> Bool
wellformed d t = freeTVars t `Set.isSubsetOf` d

freeTVars :: Type -> Set.Set Name
freeTVars (TVar x)     = Set.singleton x
freeTVars  Int         = Set.empty
freeTVars (Forall a t) = Set.delete a (freeTVars t)
freeTVars (Fun t1 t2)  = freeTVars t1 `Set.union` freeTVars t2
freeTVars (Product ts) = Set.unions (map freeTVars ts)

infer :: Term -> Maybe Type
infer = inferWith (Set.empty, Map.empty)

inferWith :: (TypeContext, ValueContext) -> Term -> Maybe Type
inferWith (d, g) = go
  where
    go (Var x) = Map.lookup x g

    go (Lit (Integer _)) = return Int

    go (App e1 e2) = do
      t <- go e1
      case t of
        Fun t1 t2 -> do
          t' <- go e2
          if t' == t1 then return t2 else Nothing
        _         -> Nothing

    go (BLam a e)
      | a `Set.member` d = Nothing
      | otherwise  = do
        t <- inferWith (Set.insert a d, g) e
        return $ Forall a t

    go (Lam (x,t) e)
      | wellformed d t = do
        t' <- inferWith (d, Map.insert x t g) e
        return $ Fun t t'
      | otherwise      = Nothing

    {-
      τ ok in Δ   Δ;Γ ⊢ e : ∀α. τ'
      ----------------------------
          Δ;Γ ⊢ e[τ] : τ'[τ/α]
    -}
    go (TApp e t)
      | wellformed d t = do
        t1 <- go e
        case t1 of
          Forall a t' -> return $ substFreeTVars (a, t) t'
          _           -> Nothing
      | otherwise = Nothing

    go (Tuple es) = do
      ts <- mapM go es
      return $ Product ts

    go (Proj e i) = do
      t <- go e
      case t of
        Product ts ->
          if 1 <= i && i <= length ts
            then return $ ts !! (i - 1)
            else Nothing
        _          -> Nothing

    go (PrimOp e1 _p e2) = do
      t1 <- go e1
      t2 <- go e2
      case (t1, t2) of
        (Int, Int) -> return Int
        _          -> Nothing

    go (If0 e1 e2 e3) = do
      t1 <- go e1
      case t1 of
        Int -> do
          t2 <- go e2
          t3 <- go e3
          if t2 == t3
            then return t2
            else Nothing
        _   -> Nothing

    go (Let recFlag bs e) = do
      checkDup (map bindId bs)
      -- TODO should also do inference for mutual rec
      bindIdTypes <- forM bs (\Bind{..} -> do { t <- typeBindId recFlag (d, g) Bind{..}
                                              ; return (bindId, t)
                                              })
      inferWith (d, Map.fromList bindIdTypes `Map.union` g) e

typeBindId :: RecFlag -> (TypeContext, ValueContext) -> Bind -> Maybe Type
typeBindId recFlag (d, g) Bind{..} = do
  (_, e, _) <- dsBind Bind{..}
  t <- inferWith (d, g) e
  case (recFlag, bindRhsAnnot) of
    (NonRec, Nothing)     -> return t
    (NonRec, Just rhsTyp) -> do
      let d' = Set.fromList bindTargs `Set.union` d
          g' = Map.fromList bindArgs `Map.union` g
      inferredRhsTyp <- inferWith (d', g') bindRhs
      if rhsTyp == inferredRhsTyp
        then return t
        else Nothing
    (Rec, Nothing)     -> Nothing
    (Rec, Just rhsTyp) -> return $ wrap Forall bindTargs $ wrap Fun [t' | (_,t') <- bindArgs] rhsTyp

--     f A1 ... An (x1 : T1) ... (xn : Tn) : T = e
-- ~~> (f, /\A1. ... /\An. \(x1 : T1). ... \(xn : Tn). e), T)
--      Provided that:
--      (1) A1, ..., An are all distinct, and
--      (2) x1, ..., xn are all distinct.
dsBind :: Bind -> Maybe (Name, Term, Maybe Type)
dsBind Bind{..} = do
  checkDup bindTargs
  checkDup [x | (x, _) <- bindArgs]
  return (bindId, def, bindRhsAnnot)
    where
      def  = (wrap BLam bindTargs . wrap Lam bindArgs) bindRhs

wrap :: (b -> a -> a) -> [b] -> a -> a
wrap cons xs t = foldr cons t xs

-- Capture-avoiding substitution
-- http://en.wikipedia.org/wiki/Lambda_calculus#Capture-avoiding_substitutions
substFreeTVars :: (Name, Type) -> Type -> Type
substFreeTVars (x, r) = go
  where
    go (TVar a)
      | a == x      = r
      | otherwise   = TVar a
    go Int          = Int
    go (Fun t1 t2)  = Fun (go t1) (go t2)
    go (Product ts) = Product (map go ts)
    go (Forall a t)
      | a == x                     = Forall a t
      | a `Set.member` freeTVars r = Forall a t -- The freshness condition
      | otherwise                  = Forall a (go t)

checkDup :: Ord a => [a] -> Maybe ()
checkDup xs =
  case findFirstDup xs of
    Just _  -> Nothing
    Nothing -> Just ()

findFirstDup :: Ord a => [a] -> Maybe a
findFirstDup xs = go xs Set.empty
  where
    go []      _ = Nothing
    go (x:xs') s = if Set.member x s
                     then Just x
                     else go xs' (Set.insert x s)
