{-# OPTIONS -XRankNTypes #-}

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
      
newtype Typ = HideTyp {revealTyp :: forall t . PFTyp t} -- type of closed types

type PrimLit = Integer -- later maybe Bool | Char

data PFExp t e = 
      FVar String e 
      
    | FBLam          (t -> PFExp t e) 
    | FLam (PFTyp t) (e -> PFExp t e) 

    | FApp  (PFExp t e) (PFExp t e)
    | FTApp (PFExp t e) (PFTyp t)

    | FPrimOp (PFExp t e) (J.Op) (PFExp t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
    | FLit PrimLit
    | FIf0 (PFExp t e) (PFExp t e) (PFExp t e)

    | FTuple [PFExp t e]
    | FProj Int (PFExp t e)

    | FFix (PFTyp t) (e -> e -> PFExp t e) (PFTyp t)  -- fix (y : t1 -> t2). \x. e

newtype Exp = HideExp {revealExp :: forall t e . PFExp t e}