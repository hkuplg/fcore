{- References for syntax:
   http://www.haskell.org/onlinereport/exps.html
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html -}

{-# LANGUAGE RecordWildCards #-}

module ESF.Syntax
  ( Name
  , Type(..)
  , eqType
  , Lit(..)
  , Term(..)
  , RecFlag(..)
  , Bind(..)
  , TypeContext
  , ValueContext
  , invariantFailed
  ) where

import qualified Language.Java.Syntax as J (Op(..))
import qualified Language.Java.Pretty as P
import Text.PrettyPrint.Leijen

import qualified Data.Map as Map
import qualified Data.Set as Set

type Name = String

data Type
  = TyVar Name
  | Int
  | Fun Type Type
  | Forall Name Type
  | Product [Type]
  deriving (Eq, Show)

eqType :: Type -> Type -> Bool
eqType = (==)

data Lit
  = Integer Integer -- later maybe Bool | Char
  deriving (Eq, Show)

data Term
  = Var Name                -- Variable
  | Lit Lit                 -- Literals
  | Lam (Name, Type) Term   -- Lambda abstraction
  | App  Term Term          -- Application
  | BLam Name Term          -- Type lambda abstraction
  | TApp Term Type          -- Type application
  | Tuple [Term]            -- Tuples
  | Proj Term Int           -- Tuple projection
  | PrimOp Term J.Op Term   -- Primitive operation
  | If0 Term Term Term      -- If expression
  | Let RecFlag [Bind] Term -- Let (rec) ... (and) ... in ...
  deriving (Show)

-- f A1 ... An (x : T1) ... (x : Tn) : T = e
data Bind = Bind
  { bindId       :: Name           -- Identifier
  , bindTargs    :: [Name]         -- Type arguments
  , bindArgs     :: [(Name, Type)] -- Arguments, each annotated with a type
  , bindRhs      :: Term           -- RHS to the "="
  , bindRhsAnnot :: Maybe Type     -- Type of the RHS
  } deriving (Show)

data RecFlag = Rec | NonRec deriving (Eq, Show)

type TypeContext  = Set.Set Name
type ValueContext = Map.Map Name Type

instance Pretty Type where
  pretty (TyVar a)    = text a
  pretty Int          = text "Int"
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ text "forall" <+> text a <> dot <+> pretty t
  pretty (Product ts) = tupled (map pretty ts)

instance Pretty Term where
  pretty (Var x) = text x
  pretty (Lit (Integer n)) = integer n
  pretty (BLam a e) = parens $ text "/\\" <> text a <> dot <+> pretty e
  pretty (Lam (x,t) e) =
    parens $
      backslash <> parens (text x <+> colon <+> pretty t) <> dot <+>
      pretty e
  pretty (TApp e t) = parens $ pretty e <+> pretty t
  pretty (App e1 e2) = parens $ pretty e1 <+> pretty e2
  pretty (Tuple es) = tupled (map pretty es)
  pretty (Proj e i) = parens (pretty e) <> text "._" <> int i
  pretty (PrimOp e1 op e2) = parens $
                               parens (pretty e1) <+>
                               text (P.prettyPrint op) <+>
                               parens (pretty e2)
  pretty (If0 e1 e2 e3) = parens $
                            text "if0" <+> pretty e1 <+>
                            text "then" <+> pretty e2 <+>
                            text "else" <+> pretty e3
  pretty (Let recFlag bs e) =
    text "let" <+> pretty recFlag <+>
    encloseSep empty empty (softline <> text "and" <> space) (map pretty bs) <+>
    text "in" <+>
    pretty e

instance Pretty Bind where
  pretty Bind{..} =
    text bindId <+>
    hsep (map pretty bindTargs) <+>
    hsep (map (\(x,t) -> parens (text x <+> colon <+> pretty t)) bindArgs) <+>
    case bindRhsAnnot of { Nothing -> empty; Just t -> colon <+> pretty t } <+>
    equals <+>
    pretty bindRhs

instance Pretty RecFlag where
  pretty Rec    = text "rec"
  pretty NonRec = empty

invariantFailed :: String -> String -> a
invariantFailed location msg = error ("Invariant failed in " ++ location ++ ": " ++ msg)
