{-# LANGUAGE FlexibleInstances, RankNTypes #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Language.SystemF.Syntax
    ( PFTyp(..)
    , PrimLit
    , PFExp(..)
    ) where

import qualified Language.Java.Syntax as J (Op(..))

data PFTyp t =
      FTVar t
    | FForall (t -> PFTyp t)
    | FFun (PFTyp t) (PFTyp t)
    | FInt
    | FProduct [PFTyp t]

instance Eq (PFTyp Int) where
    (==) = go 0
        where go i (FTVar x) (FTVar y)         = x == y
              go i (FForall f) (FForall g)     = go (i+1) (f i) (g i)
              go i (FFun s1 s2) (FFun t1 t2)   = s1 == t1 && s2 == t2
              go i FInt FInt                   = True
              go i (FProduct ss) (FProduct ts) = ss == ts
              go i _ _                         = False

newtype Typ = HideTyp {revealTyp :: forall t. PFTyp t} -- type of closed types

type PrimLit = Integer -- later maybe Bool | Char

data PFExp t e =
      FVar String e
    | FBLam          (t -> PFExp t e)
    | FLam (PFTyp t) (e -> PFExp t e)
    | FTApp (PFExp t e) (PFTyp t)
    | FApp  (PFExp t e) (PFExp t e)
    | FPrimOp (PFExp t e) (J.Op) (PFExp t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
    | FLit PrimLit
    | FIf0 (PFExp t e) (PFExp t e) (PFExp t e)
    | FTuple [PFExp t e]
    | FProj Int (PFExp t e)

    -- fix (y : t1 -> t2). \x. e
    | FFix (e -> e -> PFExp t e)
           (PFTyp t) -- t1
           (PFTyp t) -- t2, the entire expression has type t1 -> t2

newtype Exp = HideExp { revealExp :: forall t e. PFExp t e }
