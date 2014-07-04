{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Language.HM.Syntax where

data Exp = EVar String
         | ELit Lit     -- Literal
         | EApp Exp Exp
         | ELam [Pat] Exp
         | ELet (String, [Pat], Exp) Exp
         | ELetRec (String, [Pat], Exp) Exp
         | EUnOp UnOp Exp
         | EBinOp BinOp Exp Exp
         | EIf Exp Exp Exp
         | ETup [Exp]
         | EProj Exp Int
         deriving (Eq, Show)

newtype Pat = PVar String deriving (Eq, Show)

data Lit = LInteger Integer | LBool Bool deriving (Eq, Show)

data UnOp = Neg | Not deriving (Eq, Show)

data BinOp = Mul | Div | Mod
           | Add | Sub
           | Lt | Gt | Le | Ge
           | Eq | Ne
           | And 
           | Or
           deriving (Eq, Show)
                    
-- tau   ::= alpha | iota | tau -> tau
-- sigma ::= tau | forall alpha . sigma
data Type = TMono Mono
          | TPoly Poly
          deriving (Eq, Show)

-- Monotype tau
data Mono = MVar String
          | MPrim Prim  -- Primitive types
          | MApp Mono Mono -- Function Arrow: t1 -> t2
          deriving (Eq, Show)

data Prim = Int | Bool deriving (Eq, Show)

-- Polytype sigma
data Poly = PMono Mono
          | PForall [String] Poly
          deriving (Eq, Show)

{-
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

-}
