{- References for syntax:
   http://www.haskell.org/onlinereport/exps.html
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html -}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Src
  ( Module(..)
  , Type(..)
  , Expr(..), Bind(..), RecFlag(..), Lit(..), Operator(..), JCallee(..)
  , Label
  , TypeContext, ValueContext
  , Name, TcId
  -- , RdrExpr
  -- , TcBinds
  -- , TcExpr
  , deThunk
  , alphaEq
  , subtype
  , fields
  , freeTVars
  , fsubstTT
  , wrap
  , opPrec
  ) where

import JavaUtils
import PrettyUtils
import Panic

import qualified Language.Java.Syntax as J (Op(..))
-- import qualified Language.Java.Pretty as P
import Text.PrettyPrint.Leijen

import Data.Data
import qualified Data.Map as Map
import qualified Data.Set as Set

type Name       = String
type ModuleName = Name
type Label      = Name
type TcId       = (Name, Type)

data Module id = Module id [Bind id] deriving (Eq, Show)

data Type
  = TVar Name
  | JClass ClassName
  | Fun Type Type
  | Forall Name Type
  | Product [Type]
  | Record [(Label, Type)]
  | ListOf Type
  | And Type Type
  | Unit
  | Thunk Type
  -- Warning: If you ever add a case to this, you MUST also define the binary
  -- relations on your new case. Namely, add cases for your data constructor in
  -- `alphaEq` and `subtype` below.
  deriving (Eq, Show, Data, Typeable)

data Lit -- Data constructor names match Haskell types
  = Int Integer
  | String String
  | Bool Bool
  | Char Char
  | UnitLit
  deriving (Eq, Show)

data Operator = Arith J.Op | Compare J.Op | Logic J.Op deriving (Eq, Show)

data Expr id
  = Var id                              -- Variable
  | Lit Lit                             -- Literals
  | Lam (Name, Type) (Expr id)          -- Lambda
  | App  (Expr id) (Expr id)            -- Application
  | BLam Name (Expr id)                 -- Big lambda
  | TApp (Expr id) Type                 -- Type application
  | Tuple [Expr id]                     -- Tuples
  | Proj (Expr id) Int                  -- Tuple projection
  | PrimOp (Expr id) Operator (Expr id) -- Primitive operation
  | If (Expr id) (Expr id) (Expr id)    -- If expression
  | Let RecFlag [Bind id] (Expr id)     -- Let (rec) ... (and) ... in ...
  | LetOut RecFlag [(Name, Type, Expr TcId)] (Expr TcId) -- Post typecheck only
  | JNewObj ClassName [Expr id]
  | JMethod (JCallee (Expr id)) MethodName [Expr id] ClassName
  | JField  (JCallee (Expr id)) FieldName            ClassName
  | Seq [Expr id]
  | PrimList [Expr id]           -- New List
  | Merge (Expr id) (Expr id)
  | RecordLit [(Label, Expr id)]
  | RecordAccess (Expr id) Label
  | RecordUpdate (Expr id) [(Label, Expr id)]
  | LetModule (Module id) (Expr id)
  | ModuleAccess ModuleName Name
  deriving (Eq, Show)

-- type RdrExpr = Expr Name
-- type TcExpr  = Expr TcId
-- type TcBinds = [(Name, Type, Expr TcId)] -- f1 : t1 = e1 and ... and fn : tn = en

data Bind id = Bind
  { bindId       :: id             -- Identifier
  , bindTargs    :: [Name]         -- Type arguments
  , bindArgs     :: [(Name, Type)] -- Arguments, each annotated with a type
  , bindRhs      :: Expr id        -- RHS to the "="
  , bindRhsAnnot :: Maybe Type     -- Type of the RHS
  } deriving (Eq, Show)

data RecFlag = Rec | NonRec deriving (Eq, Show)

data JCallee e = Static ClassName | NonStatic e deriving (Eq, Show)

instance Functor JCallee where
  fmap _ (Static c)    = Static c
  fmap f (NonStatic e) = NonStatic (f e)

type TypeContext  = Set.Set Name
type ValueContext = Map.Map Name Type

-- Type equivalence(s) and subtyping

deThunk :: Type -> Type
deThunk (Thunk t) = deThunk t
deThunk t         = t

alphaEq :: Type -> Type -> Bool
alphaEq (TVar a)       (TVar b)       = a == b
alphaEq (JClass c)     (JClass d)     = c == d
alphaEq (Fun t1 t2)    (Fun t3 t4)    = t1 `alphaEq` t3 && t2 `alphaEq` t4
alphaEq (Forall a1 t1) (Forall a2 t2) = fsubstTT (a2, TVar a1) t2 `alphaEq` t1
alphaEq (Product ts1)  (Product ts2)  = length ts1 == length ts2 && uncurry alphaEq `all` zip ts1 ts2
alphaEq (Record fs1)   (Record fs2)   = length fs1 == length fs2
                                                && (\((l1,t1),(l2,t2)) -> l1 == l2 && t1 `alphaEq` t2) `all` zip fs1 fs2
alphaEq (ListOf t1)    (ListOf t2)    = t1 `alphaEq` t2
alphaEq (And t1 t2)    (And t3 t4)    = t1 `alphaEq` t3 && t2 `alphaEq` t4
alphaEq Unit           Unit           = True
alphaEq (Thunk t1)     t2             = t1 `alphaEq` t2
alphaEq t1             (Thunk t2)     = t1 `alphaEq` t2
alphaEq t1             t2             = False `panicOnSameDataCons` ("Src.alphaEq", t1, t2)

subtype :: Type -> Type -> Bool
subtype (TVar a)       (TVar b)       = a == b
subtype (JClass c)     (JClass d)     = c == d
  -- TODO: Should the subtype here be aware of the subtyping relations in the
  -- Java world?
subtype (Fun t1 t2)    (Fun t3 t4)    = t3 `subtype` t1 && t2 `subtype` t4
subtype (Forall a1 t1) (Forall a2 t2) = fsubstTT (a1,TVar a2) t1 `subtype` t2
subtype (Product ts1)  (Product ts2)  = length ts1 == length ts2 && uncurry subtype `all` zip ts1 ts2
subtype (Record [(l1,t1)]) (Record [(l2,t2)]) = l1 == l2 && t1 `subtype` t2
subtype (Record fs1)   (Record fs2)   = desugarMultiRecord fs1 `subtype` desugarMultiRecord fs2
subtype (ListOf t1)    (ListOf t2)    = t1 `subtype` t2  -- List :: * -> * is covariant
-- The order is significant for the two `And` cases below.
subtype t1             (And t2 t3)    = t1 `subtype` t2 && t1 `subtype` t3
subtype (And t1 t2)    t3             = t1 `subtype` t3 || t2 `subtype` t3
subtype Unit           Unit           = True
subtype t1             t2             = False `panicOnSameDataCons` ("Src.subtype", t1, t2)

-- Records

desugarMultiRecord :: [(Label,Type)] -> Type
desugarMultiRecord []         = panic "Src.desugarMultiRecordTy"
desugarMultiRecord [(l,t)]    = Record [(l,t)]
desugarMultiRecord ((l,t):fs) = Record [(l,t)] `And` desugarMultiRecord fs

fields :: Type -> [(Maybe Label, Type)]
fields t
  = case t of
      TVar _            -> unlabeledField
      JClass _          -> unlabeledField
      Fun _ _           -> unlabeledField
      Forall _ _        -> unlabeledField
      Product _         -> unlabeledField
      Unit              -> unlabeledField
      Record []         -> panic "Src.fields"
      Record [(l1,t1)]  -> [(Just l1, t1)]
      Record fs@(_:_:_) -> fields (desugarMultiRecord fs)
      ListOf _          -> unlabeledField
      (And t1 t2)       -> fields t1 ++ fields t2
      Thunk t1          -> fields t1

    where unlabeledField = [(Nothing, t)]

-- Free variable substitution

fsubstTT :: (Name, Type) -> Type -> Type
fsubstTT (x,r) (TVar a)
  | a == x                     = r
  | otherwise                  = TVar a
fsubstTT (_,_) (JClass c )     = JClass c
fsubstTT (x,r) (Fun t1 t2)     = Fun (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (Product ts)    = Product (map (fsubstTT (x,r)) ts)
fsubstTT (x,r) (Forall a t)
  | a == x                     = Forall a t
  | a `Set.member` freeTVars r = Forall a t -- The freshness condition, crucial!
  | otherwise                  = Forall a (fsubstTT (x,r) t)
fsubstTT (x,r) (ListOf a)      = ListOf (fsubstTT (x,r) a)
fsubstTT (_,_) Unit            = Unit
fsubstTT (x,r) (Record fs)     = Record (map (\(l1,t1) -> (l1, fsubstTT (x,r) t1)) fs)
fsubstTT (x,r) (And t1 t2)     = And (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (Thunk t1)      = Thunk (fsubstTT (x,r) t1)

freeTVars :: Type -> Set.Set Name
freeTVars (TVar x)     = Set.singleton x
freeTVars (JClass _)   = Set.empty
freeTVars (Fun t1 t2)  = freeTVars t1 `Set.union` freeTVars t2
freeTVars (Forall a t) = Set.delete a (freeTVars t)
freeTVars (Product ts) = Set.unions (map freeTVars ts)
freeTVars (Record fs)  = Set.unions (map (\(_l,t) -> freeTVars t) fs)
freeTVars (ListOf t)   = freeTVars t
freeTVars (And t1 t2)  = Set.union (freeTVars t1) (freeTVars t2)
freeTVars Unit         = Set.empty
freeTVars (Thunk t)    = freeTVars t

-- Pretty printers

instance Pretty Type where
  pretty (TVar a)     = text a
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ text "forall" <+> text a <> dot <+> pretty t
  pretty (Product ts) = tupled (map pretty ts)
  pretty (JClass c)   = text c
  pretty (ListOf a)   = brackets $ pretty a
  pretty (And t1 t2)  = parens (pretty t1 <+> text "&" <+> pretty t2)
  pretty _ = sorry "Core.pretty: no idea how to do"

instance Pretty id => Pretty (Expr id) where
  pretty (Var x) = pretty x
  pretty (Lit (Int n))     = integer n
  pretty (Lit (String n))  = string n
  pretty (Lit (Bool n))    = bool n
  pretty (Lit (Char n))    = char n
  pretty (Lit  UnitLit)    = unit
  pretty (BLam a e) = parens $ text "/\\" <> text a <> dot <+> pretty e
  pretty (Lam (x,t) e) =
    parens $
      backslash <> parens (pretty x <+> colon <+> pretty t) <> dot <+>
      pretty e
  pretty (TApp e t) = parens $ pretty e <+> pretty t
  pretty (App e1 e2) = parens $ pretty e1 <+> pretty e2
  pretty (Tuple es) = tupled (map pretty es)
  pretty (Proj e i) = parens (pretty e) <> text "._" <> int i
  pretty (PrimOp e1 op e2) = parens $
                               parens (pretty e1) <+>
                               text (show op) <+>
                               -- text (P.prettyPrint op) <+>
                               parens (pretty e2)
  pretty (If e1 e2 e3) = parens $
                            text "if" <+> pretty e1 <+>
                            text "then" <+> pretty e2 <+>
                            text "else" <+> pretty e3
  pretty (Let recFlag bs e) =
    text "let" <+> pretty recFlag <+>
    encloseSep empty empty (softline <> text "and" <> space) (map pretty bs) <+>
    text "in" <+>
    pretty e
  pretty (LetOut recFlag bs e) =
    text "let" <+> pretty recFlag <+>
    encloseSep empty empty (softline <> text "and" <> space)
      (map (\(f1,t1,e1) -> text f1 <+> colon <+> pretty t1 <+> equals <+> pretty e1) bs) <+>
    text "in" <+>
    pretty e
  pretty (JNewObj c args)  = text "new" <+> text c <> tupled (map pretty args)
  pretty (JMethod e m args _) = case e of (Static c)     -> pretty c  <> dot <> text m <> tupled (map pretty args)
                                          (NonStatic e') -> pretty e' <> dot <> text m <> tupled (map pretty args)
  pretty (PrimList l)         = brackets $ tupled (map pretty l)
  pretty (Merge e1 e2)  = parens (pretty e1 <+> text ",," <+> pretty e2)
  pretty _ = sorry "Src.pretty: no idea how to do"

instance Pretty id => Pretty (Bind id) where
  pretty Bind{..} =
    pretty bindId <+>
    hsep (map pretty bindTargs) <+>
    hsep (map (\(x,t) -> parens (pretty x <+> colon <+> pretty t)) bindArgs) <+>
    case bindRhsAnnot of { Nothing -> empty; Just t -> colon <+> pretty t } <+>
    equals <+>
    pretty bindRhs

instance Pretty RecFlag where
  pretty Rec    = text "rec"
  pretty NonRec = empty

-- Utilities

wrap :: (b -> a -> a) -> [b] -> a -> a
wrap cons xs t = foldr cons t xs

-- Precedence of operators based on the table in:
-- http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
opPrec :: Num a => Operator -> a
opPrec (Arith J.Mult)     = 3
opPrec (Arith J.Div)      = 3
opPrec (Arith J.Rem)      = 3
opPrec (Arith J.Add)      = 4
opPrec (Arith J.Sub)      = 4
opPrec (Compare J.LThan)  = 6
opPrec (Compare J.GThan)  = 6
opPrec (Compare J.LThanE) = 6
opPrec (Compare J.GThanE) = 6
opPrec (Compare J.Equal)  = 7
opPrec (Compare J.NotEq)  = 7
opPrec (Logic J.CAnd)     = 11
opPrec (Logic J.COr)      = 12
opPrec op = panic $ "Src.Syntax.opPrec: " ++ show op
