module SystemF.TypeInferer where

import SystemF.Syntax

type TEnv = [(String, PFTyp Int)]
type Env  = [(String, PFExp Int Int)]

infer :: (TEnv, Env) -> PFExp Int Int -> PFTyp Int
infer (tenv, env) e = case e of
    FVar x -> error ""
    FBLam f -> error ""
    FLam t f -> error ""
    FApp e1 e2 -> error ""
    FTApp e t -> error ""
    FPrimOp e1 op e2 -> error ""
    FLit n -> PFInt
    Fif0 e1 e2 e3 -> error ""
    FTuple es -> error ""
    FProj i e -> 
        case e of FTuple es -> infer (tenv, env) (es !! i)
                  _         -> error ""
    FFix t1 f t2 -> error ""
