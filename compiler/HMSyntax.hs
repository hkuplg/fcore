-- We need this module to break circular dependency, namely,
-- the parser needs the syntax definition and we'd like to import the
-- parser in the module HM.
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module HMSyntax where

type Var = String
type TVar = String

-- e ::= x | e e' | \x . e | let x = e in e'
data Exp = EVar Var
         | ELit Lit     -- Literal
         | EApp Exp Exp
         | ELam Var Exp
         | ELet Var Exp Exp
         | ELetRec [(Var, Exp)] Exp  -- let x1 = e1 and x2 = e2 and ... in e
         | EUn UnOp Exp
         | EBin BinOp Exp Exp
         | EIf Exp Exp Exp
         deriving (Eq, Show)

type Lit = Int 

data UnOp = UMinus | Not deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Div | Mod 
           | Eq | Ne | Lt | Gt | Le | Ge 
           | And | Or
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

data Prim = Int | Bool deriving (Eq, Show)

-- Polytype sigma
data Poly = PMono Mono
          | PForall TVar Poly
          deriving (Show)

-- Equality of monotypes is purely syntactical. But syntactically different
-- polytypes are equal w.r.t. renaming their quantified variables.
instance Eq Poly where
    PMono t1 == PMono t2           = t1 == t2
    PMono _     == PForall _ _     = False
    PForall _ _ == PMono _         = False
    PForall a1 s1 == PForall a2 s2 = (a1 == a2 && s1 == s2) || eq [(a1,a2)] s1 s2
        where
            eq rs (PForall a1 s1) (PForall a2 s2) = eq ((a1,a2):rs) s1 s2
            eq rs (PMono t1) (PMono t2)           = eqMono rs t1 t2
                where 
                    eqMono rs (MVar a1) (MVar a2)       = case lookup a1 rs of Nothing  -> a1  == a2
                                                                               Just a1' -> a1' == a2
                    eqMono rs (MPrim t1)   (MPrim t2)   = t1 == t2
                    eqMono rs (MApp s1 t1) (MApp s2 t2) = eqMono rs s1 s2 && eqMono rs t1 t2
                    eqMono rs _            _            = False
            eq rs _ _                             = False

