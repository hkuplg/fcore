{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module SystemF.TypeInference (inferType) where

import SystemF.Syntax

inferType :: (t1, t2, t3) -> PFExp t e -> PFTyp t
inferType (tenv, env, venv) e = 
    case e of
        FVar x          -> error "FVar"
        FBLam f         -> FForall (inferType (tenv, env, venv) . f)
        FLam t f        -> error "FLam" -- FFun t (inferType (tenv, env, venv) $ f 0)
        FApp e1 e2      -> let te1 = inferType (tenv, env, venv) e1
                               te2 = inferType (tenv, env, venv) e2
                           in
                           case te1 of FFun t1 t2 {- | te2 == t1 -} -> error "Function application OK" -- TODO
                                       otherwise -> error "Function application not OK"
        FTApp e t        -> error ""
        FPrimOp e1 op e2 -> let te1 = inferType (tenv, env, venv) e1
                                te2 = inferType (tenv, env, venv) e2
                            in
                            if True -- (te1 :: PFTyp Int) == PFInt && (te2 :: PFTyp Int) == PFInt  -- TODO
                                then PFInt 
                                else error "Type mismatch in primitive op"
        FLit n           -> PFInt
        Fif0 e1 e2 e3 -> inferType (tenv, env, venv) e2         -- TODO
        FTuple es -> Tuple $ map (inferType (tenv, env, venv)) es
        FProj i e -> 
            case e of FTuple es | i <= length es -> inferType (tenv, env, venv) (es !! (i - 1))
                      otherwise -> error "Projection on a non-tuple"
        FFix t1 f t2 -> FFun t1 t2
