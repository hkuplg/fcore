> {-# OPTIONS -XFlexibleInstances #-}

> module ParsePhoas where

> import Data.List
> import Data.Maybe

> data PExp e = Var e | Lam (e -> PExp e) | App (PExp e) (PExp e)

> instance Show (PExp Int) where
>  show p = go p 0 where
>    go (Var x) n = show x
>    go (Lam f) n = "(\\x" ++ show n ++ ". " ++ go (f n) (n+1) ++ ")"
>    go (App e1 e2) n = go e1 n ++ " " ++ go e2 n

> term = "\\x.x"

> parseExp :: String -> [(Char,e)] -> PExp e
> parseExp ('\\' : 'x' : '.' : ss) env = Lam (\x -> parseExp ss (('x',x) : env) )
> parseExp ('x' : ss) env = Var (fromJust (lookup 'x' env))