{- References for syntax:
   http://www.haskell.org/onlinereport/exps.html
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html -}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Src
  ( Type(..)
  , Expr(..), Bind(..), RecFlag(..), Lit(..), Operator(..)
  , Label
  , TypeContext, ValueContext
  , Name, TcId
  -- , RdrExpr
  -- , TcBinds
  -- , TcExpr
  , alphaEquiv
  , subtype
  , fields
  , freeTyVars
  , substFreeTyVars
  , wrap
  , opPrec
  , unwrapOp
  , opReturnType
  ) where

import JavaUtils
import Panic

import qualified Language.Java.Syntax as J (Op(..))
-- import qualified Language.Java.Pretty as P
import Text.PrettyPrint.Leijen

import Data.Data
import qualified Data.Map as Map
import qualified Data.Set as Set

type Name  = String
type Label = Name
type TcId  = (Name, Type)

data Type
  = TyVar Name
  | JClass ClassName
  | Fun Type Type
  | Forall Name Type
  | Product [Type]
  | RecordTy [(Label, Type)]
  | ListOf Type
  | And Type Type
  -- Warning: If you ever add a case to this, you MUST also define the binary
  -- relations on your new case. Namely, add cases for your data constructor in
  -- `alphaEquiv` and `subtype` below.
  deriving (Eq, Show, Data, Typeable)

data Lit
    = Integer Integer
    | String String
    | Boolean Bool
    | Char Char
    deriving (Eq, Show)

data Operator = Arith J.Op | Compare J.Op | Logic J.Op
    deriving (Eq, Show)

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
  | JMethod (Either ClassName (Expr id)) MethodName [Expr id] ClassName
  | JField  (Either ClassName (Expr id)) FieldName            ClassName
  | Seq [Expr id]
  | PrimList [Expr id]           -- New List
  | Merge (Expr id) (Expr id)
  | Record [(Label, Expr id)]
  | RecordAccess (Expr id) Label
  | RecordUpdate (Expr id) [(Label, Expr id)]
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

type TypeContext  = Set.Set Name
type ValueContext = Map.Map Name Type

-- Type equivalence(s) and subtyping

alphaEquiv :: Type -> Type -> Bool
alphaEquiv (TyVar a)      (TyVar b)      = a == b
alphaEquiv (JClass c)     (JClass d)     = c == d
alphaEquiv (Fun t1 t2)    (Fun t3 t4)    = t1 `alphaEquiv` t3 && t2 `alphaEquiv` t4
alphaEquiv (Forall a1 t1) (Forall a2 t2) = substFreeTyVars (a2, TyVar a1) t2 `alphaEquiv` t1
alphaEquiv (Product ts1)  (Product ts2)  = length ts1 == length ts2 && uncurry alphaEquiv `all` zip ts1 ts2
alphaEquiv (RecordTy fs1) (RecordTy fs2) = length fs1 == length fs2
                                          && (\((l1,t1),(l2,t2)) -> l1 == l2 && t1 `alphaEquiv` t2) `all` zip fs1 fs2
alphaEquiv (ListOf t1)    (ListOf t2)    = t1 `alphaEquiv` t2
alphaEquiv (And t1 t2)    (And t3 t4)    = t1 `alphaEquiv` t3 && t2 `alphaEquiv` t4
alphaEquiv t1             t2             = falseIfDataConsDiffer "Src.alphaEquiv" t1 t2

subtype :: Type -> Type -> Bool
subtype (TyVar a)      (TyVar b)      = a == b
subtype (JClass c)     (JClass d)     = c == d
  -- TODO: Should the subtype here be aware of the subtyping relations in the
  -- Java world?
subtype (Fun t1 t2)    (Fun t3 t4)    = t3 `subtype` t1 && t2 `subtype` t4
subtype (Forall a1 t1) (Forall a2 t2) = substFreeTyVars (a1, TyVar a2) t1 `subtype` t2
subtype (Product ts1)  (Product ts2)  = length ts1 == length ts2 && uncurry subtype `all` zip ts1 ts2
subtype (RecordTy [(l1,t1)]) (RecordTy [(l2,t2)]) = l1 == l2 && t1 `subtype` t2
subtype (RecordTy fs1)       (RecordTy fs2)       = desugarMultiRecordTy fs1 `subtype` desugarMultiRecordTy fs2
subtype (ListOf t1)    (ListOf t2)    = t1 `subtype` t2  -- List :: * -> * is covariant
subtype (And t1 t2)    t3             = t1 `subtype` t3 || t2 `subtype` t3
subtype t1             (And t2 t3)    = t1 `subtype` t2 && t1 `subtype` t3
subtype t1             t2             = falseIfDataConsDiffer "Src.subtype" t1 t2

-- Records

desugarMultiRecordTy :: [(Label,Type)] -> Type
desugarMultiRecordTy []         = panic "Src.desugarMultiRecordTy"
desugarMultiRecordTy [(l,t)]    = RecordTy [(l,t)]
desugarMultiRecordTy ((l,t):fs) = RecordTy [(l,t)] `And` desugarMultiRecordTy fs

fields :: Type -> [(Maybe Label, Type)]
fields t
  = case t of
      TyVar _             -> unlabeledField
      JClass _            -> unlabeledField
      Fun _ _             -> unlabeledField
      Forall _ _          -> unlabeledField
      Product _           -> unlabeledField
      RecordTy []         -> panic "Src.fields"
      RecordTy [(l1,t1)]  -> [(Just l1, t1)]
      RecordTy fs@(_:_:_) -> fields (desugarMultiRecordTy fs)
      ListOf _            -> unlabeledField
      (And t1 t2)         -> fields t1 ++ fields t2

      where unlabeledField = [(Nothing, t)]

-- Free variable substitution

substFreeTyVars :: (Name, Type) -> Type -> Type
substFreeTyVars (x, r) = go
  where
    go (TyVar a)
      | a == x      = r
      | otherwise   = TyVar a
    go (JClass c )  = JClass c
    go (Fun t1 t2)  = Fun (go t1) (go t2)
    go (Product ts) = Product (map go ts)
    go (Forall a t)
      | a == x                      = Forall a t
      | a `Set.member` freeTyVars r = Forall a t -- The freshness condition, crucial!
      | otherwise                   = Forall a (go t)
    go (ListOf a)   = ListOf (go a)

freeTyVars :: Type -> Set.Set Name
freeTyVars (TyVar x)     = Set.singleton x
freeTyVars (JClass _)    = Set.empty
freeTyVars (Fun t1 t2)   = freeTyVars t1 `Set.union` freeTyVars t2
freeTyVars (Forall a t)  = Set.delete a (freeTyVars t)
freeTyVars (Product ts)  = Set.unions (map freeTyVars ts)
freeTyVars (RecordTy fs) = Set.unions (map (\(_l,t) -> freeTyVars t) fs)
freeTyVars (ListOf t)    = freeTyVars t
freeTyVars (And t1 t2)   = Set.union (freeTyVars t1) (freeTyVars t2)

-- Pretty printers

instance Pretty Type where
  pretty (TyVar a)    = text a
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ text "forall" <+> text a <> dot <+> pretty t
  pretty (Product ts) = tupled (map pretty ts)
  pretty (JClass c)   = text c
  pretty (ListOf a)   = brackets $ pretty a
  pretty (And t1 t2)  = parens (pretty t1 <+> text "&" <+> pretty t2)

instance Pretty id => Pretty (Expr id) where
  pretty (Var x) = pretty x
  pretty (Lit (Integer n)) = integer n
  pretty (Lit (String n)) = string n
  pretty (Lit (Boolean n)) = bool n
  pretty (Lit (Char n)) = char n
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
  pretty (JMethod e m args _) = case e of (Left e')  -> pretty e' <> dot <> text m <> tupled (map pretty args)
                                          (Right e') -> pretty e' <> dot <> text m <> tupled (map pretty args)
  pretty (PrimList l)         = brackets $ tupled (map pretty l)
  pretty (Merge e1 e2)  = parens (pretty e1 <+> text ",," <+> pretty e2)

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

unwrapOp :: Operator -> J.Op
unwrapOp (Arith op)   = op
unwrapOp (Compare op) = op
unwrapOp (Logic op)   = op

opReturnType :: Operator -> Type
opReturnType (Arith _)   = JClass "java.lang.Integer"
opReturnType (Compare _) = JClass "java.lang.Boolean"
opReturnType (Logic _)   = JClass "java.lang.Boolean"