-- Hindley–Milner type system
-- http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner
-- http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf
module HM where

import Data.List        (union, delete)

type Var = String
type TVar = String

-- e ::= x | e e' | \x . e | let x = e in e'
data Exp = EVar Var
         | EApp Exp Exp
         | ELam Var Exp
         | ELet Var Exp Exp
         deriving (Eq, Show)

-- tau   ::= alpha | iota | tau -> tau
-- sigma ::= tau | forall alpha . sigma
data Type = TMono Mono
          | TPoly Poly
          deriving (Eq, Show)

-- Monotype tau
data Mono = MVar TVar
          | MPrim Prim  -- Primitive types
          | MApp Mono Mono
          deriving (Eq, Show)

data Prim = PInt | PBool deriving (Eq, Show)

-- Polytype sigma
data Poly = PMono Mono
          | PForall TVar Poly
          deriving (Eq, Show)

type Env = [(Var, Type)]

-- Free Type Variables
free :: Type -> [TVar]
free (TMono (MVar a)) = [a]
free (TMono (MApp t1 t2)) = free (TMono t1) `union` free (TMono t2)
free (TPoly (PForall a poly)) = delete a (free (TPoly poly))

type Subst = [(Mono, TVar)]

-- TODO: implement
subst :: Subst -> Poly -> Poly
subst _ x = x

-- TODO: implement
lessSpecialThan :: Poly -> Poly -> Bool
lessSpecialThan s1 s2 = True

typeExp :: Env -> Exp -> Maybe Type
typeExp env (EVar x) = lookup x env
typeExp env (EApp e0 e1) = do
    t0 <- typeExp env e0
    t1 <- typeExp env e1
    case t0 of 
        (TMono (MApp t t')) -> if t1 == TMono t then Just (TMono t') else Nothing
        _ -> Nothing
typeExp env (ELam x e) = error "typeExp env (ELam x e)" -- TODO
typeExp env (ELet x e0 e1) = do
    t0 <- typeExp env e0
    typeExp ((x, t0):env) e1
-- TODO: missing two cases: [Inst] and [Gen]

prettyExp :: Exp -> String
prettyExp (EVar x) = x
prettyExp (EApp e0 e1) = "(" ++ prettyExp e0 ++ " " ++ prettyExp e1 ++ ")"
prettyExp (ELam x e) = "(\\" ++ x ++ " . " ++ prettyExp e ++ ")"
prettyExp (ELet x e0 e1) = "(let " ++ x ++ " = " ++ prettyExp e0 ++ " in " ++ prettyExp e1 ++ ")"

foo :: Exp
foo = ELam "y" (EVar "x")

bar :: Exp
bar = ELam "x" (ELet "foo" foo (EVar "foo"))

example1 :: Exp
example1 = ELet "bar" bar (EVar "bar")
