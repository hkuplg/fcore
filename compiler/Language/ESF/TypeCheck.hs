{-# OPTIONS_GHC -fwarn-unused-binds #-}
{-# LANGUAGE RecordWildCards #-}

{- "GHC type checks programs in their original Haskell form before the desugarer converts
   them into Core code. This complicates the type checker as it has to handle the much more
   verbose Haskell AST, but it improves error messages, as those message are based on the
   same structure that the user sees." -}

module Language.ESF.TypeCheck where

import Language.ESF.Syntax

import Control.Monad

import Text.PrettyPrint.Leijen

import Data.Maybe       (fromJust)
import Data.List        (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf

-- The monad for typechecking
type Tc = Either TypeError

data TypeError
  = NotInScope { msg      :: String }
  | General    { msg      :: String }
  | Mismatch   { term     :: Term
               , expected :: Type
               , actual   :: Type
               }

instance Pretty TypeError where
  pretty Mismatch{..} =
    text "Type mismatch:" <$>
    text "Expected:" <+> pretty expected <$>
    text "Actual:"   <+> pretty actual <$>
    text "In the expression:" <+> pretty term
  pretty NotInScope{..} = text "Not in scope:" <+> string msg
  pretty General{..}    = string msg

q :: String -> String
q x = "`" ++ x ++ "'"

checkWellformed :: TypeContext -> Type -> Tc ()
checkWellformed d t =
  let s = freeTyVars t `Set.difference` d in
  unless (Set.null s) $
    Left
      NotInScope { msg = "type variable" ++
                         (if Set.size s > 1 then "s" else "") ++
                         " " ++ intercalate ", " (map q (Set.toList s))
                 }

freeTyVars :: Type -> Set.Set Name
freeTyVars (TyVar x)    = Set.singleton x
freeTyVars  Int         = Set.empty
freeTyVars (Forall a t) = Set.delete a (freeTyVars t)
freeTyVars (Fun t1 t2)  = freeTyVars t1 `Set.union` freeTyVars t2
freeTyVars (Product ts) = Set.unions (map freeTyVars ts)

infer :: Term -> Tc Type
infer = inferWith (Set.empty, Map.empty)

inferWith :: (TypeContext, ValueContext) -> Term -> Tc Type
inferWith (d, g) = go
  where
    go (Var x) =
      case Map.lookup x g of
        Just t  -> return t
        Nothing -> Left NotInScope { msg = "variable " ++ q x }

    go (Lit (Integer _)) = return Int

    go (App e1 e2) = do
      t  <- go e1
      t' <- go e2
      case t of
        Fun t1 t2 | t' == t1 -> return t2
        Fun t1 _ -> Left Mismatch { term = e2, expected = t1, actual = t' }
        _        -> Left Mismatch { term = e1
                                  , expected = Fun t' (TyVar "_")
                                  , actual = t
                                  }

    go (BLam a e)
      | a `Set.member` d = Left
                            General { msg = "This type variable " ++ q a ++
                                            " shadows an existing type variable" ++
                                            " in an outer scope"
                                    }
      | otherwise        = do t <- inferWith (Set.insert a d, g) e
                              return $ Forall a t

    go (Lam (x,t) e) = do
      checkWellformed d t
      t' <- inferWith (d, Map.insert x t g) e
      return (Fun t t')

    {-
      τ ok in Δ   Δ;Γ ⊢ e : ∀α. τ'
      ----------------------------
          Δ;Γ ⊢ e[τ] : τ'[τ/α]
    -}
    go (TApp e t) = do
      checkWellformed d t
      t1 <- go e
      case t1 of
        Forall a t' -> return $ substFreeTyVars (a, t) t'
        _           -> Left Mismatch { term     = TApp e t
                                     , expected = Forall "_" (TyVar "_")
                                     , actual   = t1
                                     }

    go (Tuple es)
      | length es < 2 = invariantFailed
                          "inferWith"
                          ("fewer than two items in the tuple " ++ show (Tuple es))
      | otherwise     = do ts <- mapM go es
                           return $ Product ts

    go (Proj e i) = do
      t <- go e
      case t of
        Product ts ->
          if 1 <= i && i <= length ts
            then return $ ts !! (i - 1)
            else Left General { msg = "Index too large in projection" }
        _          ->
          Left General { msg = "Projection of a term that is not of product type" }

    go (PrimOp e1 _p e2) = do
      t1 <- go e1
      t2 <- go e2
      case (t1, t2) of
        (Int, Int) -> return Int
        (Int, _  ) -> Left Mismatch { term = e2, expected = Int, actual = t2 }
        (_  , _  ) -> Left Mismatch { term = e1, expected = Int, actual = t1 }

    go (If0 e1 e2 e3) = do
      t1 <- go e1
      case t1 of
        Int -> do
          t2 <- go e2
          t3 <- go e3
          if t2 == t3
            then return t2
            else Left Mismatch { term = e3, expected = t2, actual = t3 }
        _   -> Left Mismatch { term = e1, expected = Int, actual = t1 }

    -- TODO: refactor Let Rec and Let NonRec
    go (Let Rec bs e) = do
      checkForDup "identifiers" (map bindId bs)
      sigs <-
        foldM
          (\acc Bind{..} -> do
            checkForDup "type arguments" bindTargs
            checkForDup "arguments"      [x | (x, _) <- bindArgs]
            case bindRhsAnnot of
              Nothing ->
                Left General { msg = "Missing type annotation for the right hand side" }
              Just rhsTyp ->
                return $
                  Map.insert
                    bindId
                    (wrap Forall bindTargs $ wrap Fun [t | (_,t) <- bindArgs] rhsTyp)
                    acc)
          Map.empty
          bs
      forM_
        bs
        (\Bind{..} -> do
          let d_local = Set.fromList bindTargs
              g_local = Map.fromList bindArgs
          inferredRhsTyp <- inferWith
                              ( d_local `Set.union` d
                              , g_local `Map.union` sigs `Map.union` g
                              )
                              bindRhs
          unless (fromJust bindRhsAnnot `eqType` inferredRhsTyp) $
            Left Mismatch { term     = bindRhs
                          , expected = fromJust bindRhsAnnot
                          , actual   = inferredRhsTyp
                          })
      inferWith (d, sigs `Map.union` g) e

    go (Let NonRec bs e) = do
      checkForDup "identifiers" (map bindId bs)
      defined <-
        foldM
          (\acc Bind{..} -> do
            let d_local = Set.fromList bindTargs
                g_local = Map.fromList bindArgs
            inferredRhsTyp <- inferWith
                                ( d_local `Set.union` d
                                , g_local `Map.union` g
                                )
                                bindRhs
            case bindRhsAnnot of
              Nothing -> return ()
              Just claimedRhsTyp -> do
                unless (claimedRhsTyp `eqType` inferredRhsTyp) $
                  Left Mismatch { term     = bindRhs
                                , expected = claimedRhsTyp
                                , actual   = inferredRhsTyp
                                }
            return $
              Map.insert
                bindId
                (wrap Forall bindTargs $
                  wrap Fun [t | (_,t) <- bindArgs]
                    inferredRhsTyp)
                acc)
          Map.empty
          bs
      inferWith (d, defined `Map.union` g) e

-- TODO: BUG: if any of A1, ..., An is in the free variables of t1, ..., tn
{-      f A1 ... An (x1 : t1) ... (xn : tn) : t = e
        ~> (f, /\A1. ... /\An. \(x1 : t1). ... \(xn : tn). e), t)

        Provided that:
          (1) A1, ..., An are all distinct, and
          (2) x1, ..., xn are all distinct.     -}
dsBind :: Bind -> Tc (Name, Term, Maybe Type)
dsBind Bind{..} = do
  checkForDup "type arguments" bindTargs
  checkForDup "arguments"      [x | (x, _) <- bindArgs]
  return (bindId, def, bindRhsAnnot)
    where
      def  = (wrap BLam bindTargs . wrap Lam bindArgs) bindRhs

wrap :: (b -> a -> a) -> [b] -> a -> a
wrap cons xs t = foldr cons t xs

-- Capture-avoiding substitution
-- http://en.wikipedia.org/wiki/Lambda_calculus#Capture-avoiding_substitutions
substFreeTyVars :: (Name, Type) -> Type -> Type
substFreeTyVars (x, r) = go
  where
    go (TyVar a)
      | a == x      = r
      | otherwise   = TyVar a
    go Int          = Int
    go (Fun t1 t2)  = Fun (go t1) (go t2)
    go (Product ts) = Product (map go ts)
    go (Forall a t)
      | a == x                      = Forall a t
      | a `Set.member` freeTyVars r = Forall a t -- The freshness condition, crucial!
      | otherwise                   = Forall a (go t)

checkForDup :: String -> [Name] -> Tc ()
checkForDup what xs =
  case findFirstDup xs of
    Just x  -> Left General { msg = "Duplicate " ++ what ++ ": " ++ q x }
    Nothing -> return ()

findFirstDup :: Ord a => [a] -> Maybe a
findFirstDup xs = go xs Set.empty
  where
    go []      _ = Nothing
    go (x:xs') s = if Set.member x s
                     then Just x
                     else go xs' (Set.insert x s)
