module SystemF where

import SystemF.Syntax

-- (1, /\A. \(x : A). x)
pair :: PFExp Int Int
pair = FTuple [FLit 1, FBLam (\a -> FLam (FTVar a) (\x -> FVar "" x))]
