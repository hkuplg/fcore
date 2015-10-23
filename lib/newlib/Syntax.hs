{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, OverloadedStrings  #-}

module Syntax where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Unbound.Generics.LocallyNameless

import           JavaUtils
import qualified Language.Java.Syntax as J
import qualified Src as S



type TmName = Name Expr

data Tele = Empty
          | Cons (Rebind (TmName, Embed Expr) Tele)
  deriving (Show, Generic, Typeable)

-- | Syntax of the core, with optimization of aggregate bindings
data Expr = Var TmName
          | App Expr Expr
          | Lam (Bind Tele Expr)
          | Pi (Bind Tele Expr)
          | Mu (Bind (TmName, Embed Expr) Expr)
          | F Expr Expr
          | U Expr
          | Star

          | Let (Bind (TmName, Embed Expr) Expr)
          | If Expr Expr Expr
          | Nat
          | Lit S.Lit
          | PrimOp S.Operator Expr Expr

          | JClass ClassName
          | Unit


  deriving (Show, Generic, Typeable)

data Operation = Mult
               | Sub
               | Add
  deriving (Show, Generic, Typeable)

-- addExpr :: Expr -> Expr -> Expr
-- addExpr = PrimOp Add

-- subExpr :: Expr -> Expr -> Expr
-- subExpr = PrimOp Sub

-- multExpr :: Expr -> Expr -> Expr
-- multExpr = PrimOp Mult

instance Alpha Expr
instance Alpha S.Lit
instance Alpha S.Operator
instance Alpha J.Op
instance Alpha Operation
instance Alpha Tele

instance Subst Expr Operation
instance Subst Expr S.Operator
instance Subst Expr S.Lit
instance Subst Expr J.Op
instance Subst Expr Tele

instance Subst Expr Expr where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing



-- Examples

-- \ x : ⋆ . \ y : x . y
-- polyid :: Expr
-- polyid = elam [("x", estar), ("y", evar "x")] (evar "y")


-- pi x : ⋆ . x -> x
-- polyidty :: Expr
-- polyidty = epi [("x", estar)] (earr (evar "x") (evar "x"))

-- castup [(\ x : * . x) int] 3
-- castupint :: Expr
-- castupint = F (eapp (elam [("x", estar)] (evar "x")) Nat) (Lit 3)


-- smart constructors

evar :: String -> Expr
evar = Var . string2Name

elam :: [(String, Expr)] -> Expr -> Expr
elam t b = Lam (bind (mkTele t) b)

emu :: (String, Expr) -> Expr -> Expr
emu (n, t) b = Mu (bind (string2Name n, embed t) b)

epi :: [(String, Expr)] -> Expr -> Expr
epi t b = Pi (bind (mkTele t) b)

earr :: Expr -> Expr -> Expr
earr t1 = epi [("_", t1)]

estar :: Expr
estar = Star

eapp :: Expr -> Expr -> Expr
eapp = App

elet :: String -> Expr -> Expr -> Expr
elet n e1 e2 = Let (bind (s2n n, embed e1) e2)

mkTele :: [(String, Expr)] -> Tele
mkTele []          = Empty
mkTele ((x,e) : t) = Cons (rebind (string2Name x, Embed e) (mkTele t))
