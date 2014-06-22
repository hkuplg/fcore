module SystemF.Syntax where

import qualified Language.Java.Syntax as J (Op (..))

-- System F syntax

data PFTyp t = FTVar t 
             | FForall (t -> PFTyp t) 
             | FFun (PFTyp t) (PFTyp t) 
             | PFInt

type PrimLit = Integer -- later maybe Bool | Char

data PFExp t e = 
     FVar e 
   | FBLam (t -> PFExp t e) 
   | FLam (PFTyp t) (e -> PFExp t e) 
   | FApp (PFExp t e) (PFExp t e)
   | FTApp (PFExp t e) (PFTyp t)
   | FPrimOp (PFExp t e) (J.Op) (PFExp t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
   | FLit PrimLit
   | Fif0 (PFExp t e) (PFExp t e) (PFExp t e)
   | FTuple [PFExp t e]
   | FProj Int (PFExp t e)
   -- fixpoints
   | FFix (PFTyp t) (e -> e -> PFExp t e) (PFTyp t)  -- fix y. \(x : t1). e : t2
