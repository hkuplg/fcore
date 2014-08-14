{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module SystemF.Syntax
    ( PFTyp(..)
    , PFExp(..)
    ) where

import ESF.Syntax

data PFTyp t =
      FTVar t
    | FForall (t -> PFTyp t)
    | FFun (PFTyp t) (PFTyp t)
    | FJClass String
    | FProduct [PFTyp t]

instance Eq (PFTyp Int) where
    (==) = go 0
        where go i (FTVar x) (FTVar y)         = x == y
              go i (FForall f) (FForall g)     = go (i+1) (f i) (g i)
              go i (FFun s1 s2) (FFun t1 t2)   = s1 == t1 && s2 == t2
              go i (FJClass c1) (FJClass c2)   = c1 == c2
              go i (FProduct ss) (FProduct ts) = ss == ts
              go i _ _                         = False

newtype Typ = HideTyp {revealTyp :: forall t. PFTyp t} -- type of closed types


data PFExp t e =
      FVar String e
    | FBLam          (t -> PFExp t e)
    | FLam (PFTyp t) (e -> PFExp t e)
    | FTApp (PFExp t e) (PFTyp t)
    | FApp  (PFExp t e) (PFExp t e)
    | FPrimOp (PFExp t e) Operator (PFExp t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
    | FLit Lit
    | FIf (PFExp t e) (PFExp t e) (PFExp t e)
    | FTuple [PFExp t e]
    | FProj Int (PFExp t e)

    -- fix x (x1 : t1) : t2. e
    -- Or the new syntax:
    -- fix (x : t1 -> t2). \x1. e
    | FFix (e -> e -> PFExp t e)
           (PFTyp t) -- t1
           (PFTyp t) -- t2
    -- Java
    | FJNewObj String [PFExp t e]
    | FJMethod (PFExp t e) String [PFExp t e]

newtype Exp = HideExp { revealExp :: forall t e. PFExp t e }
