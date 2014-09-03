{- References for syntax:
   http://www.haskell.org/onlinereport/exps.html
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html -}

{-# LANGUAGE RecordWildCards #-}

module Src
  ( Type(..)
  , Expr(..), Bind(..), RecFlag(..), Lit(..), Operator(..)
  , TypeContext, ValueContext
  , Name, TcId
  -- , RdrExpr
  -- , TcBinds
  -- , TcExpr
  , alphaEqTy
  , subtype
  , freeTyVars
  , substFreeTyVars
  , wrap
  , opPrec
  , unwrapOp
  ) where

import JavaUtils
import Panic

import qualified Language.Java.Syntax as J (Op(..))
-- import qualified Language.Java.Pretty as P
import Text.PrettyPrint.Leijen

import qualified Data.Map as Map
import qualified Data.Set as Set

type Name = String
type TcId = (Name, Type)

data Type
  = TyVar Name
  | Fun Type Type
  | Forall Name Type
  | Product [Type]
  | JClass ClassName
  | And Type Type
  deriving (Eq, Show)

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
  | Merge (Expr id) (Expr id)
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

alphaEqTy :: Type -> Type -> Bool
alphaEqTy (TyVar a)      (TyVar b)      = a == b
alphaEqTy (JClass a)     (JClass b)     = a == b
alphaEqTy (Fun t1 t2)    (Fun t3 t4)    = t1 `alphaEqTy` t3 && t2 `alphaEqTy` t4
alphaEqTy (Product ts1)  (Product ts2)  = length ts1 == length ts2 &&
                                          uncurry alphaEqTy `all` zip ts1 ts2
alphaEqTy (Forall a1 t1) (Forall a2 t2) = substFreeTyVars (a2, TyVar a1) t2 `alphaEqTy` t1
alphaEqTy  _              _             = False

subtype :: Type -> Type -> Bool
subtype t1 t2
  | t1 `alphaEqTy` t2 = True -- Refl
  | otherwise =
    case (t1, t2) of
      (And t1_1 t1_2, _) -> t1_1 `subtype` t2 || t1_2 `subtype` t2
      (_, And t2_1 t2_2) -> t1 `subtype` t2_1 && t1 `subtype` t2_2
      (_, _)             -> False

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

freeTyVars :: Type -> Set.Set Name
freeTyVars (TyVar x)    = Set.singleton x
freeTyVars (JClass _)   = Set.empty
freeTyVars (Forall a t) = Set.delete a (freeTyVars t)
freeTyVars (Fun t1 t2)  = freeTyVars t1 `Set.union` freeTyVars t2
freeTyVars (Product ts) = Set.unions (map freeTyVars ts)

instance Pretty Type where
  pretty (TyVar a)    = text a
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ text "forall" <+> text a <> dot <+> pretty t
  pretty (Product ts) = tupled (map pretty ts)
  pretty (JClass c)   = text c
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