-- References for syntax:
-- http://www.haskell.org/onlinereport/exps.html
-- http://caml.inria.fr/pub/docs/manual-ocaml/expr.html

module Language.ESF.Syntax where

import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Java.Syntax as J (Op(..))
import Text.PrettyPrint.Leijen

type Name = String

data Type
  = TVar Name
  | Int
  | Fun Type Type
  | Forall Name Type
  | Product [Type]
  deriving (Eq, Show)

data Lit
  = Integer Integer -- later maybe Bool | Char
  deriving (Eq, Show)

data Term
  = Var Name                     -- Variable
  | Lit Lit                      -- Literals
  | Lam (Name, Type) Term        -- Lambda abstraction
  | App  Term Term               -- Application
  | BLam Name Term               -- Type lambda abstraction
  | TApp Term Type               -- Type application
  | Tuple [Term]                 -- Tuples
  | Proj Term Int                -- Tuple projection
  | PrimOp Term J.Op Term        -- Primitive operation
  | If0 Term Term Term           -- If expression
  | Let RecFlag [Bind] Term -- Let (rec) ... (and) ... in ...
  deriving (Eq, Show)

-- f A1 ... An (x : T1) ... (x : Tn) : T = e
data Bind = Bind
  { bindId       :: Name           -- Identifier
  , bindTargs    :: [Name]         -- Type arguments
  , bindArgs     :: [(Name, Type)] -- Arguments, each annotated with a type
  , bindRhs      :: Term           -- RHS to the "="
  , bindRhsAnnot :: Maybe Type     -- Type of the RHS
  } deriving (Eq, Show)

data RecFlag = Rec | NonRec deriving (Eq, Show)

type TypeContext  = Set.Set Name
type ValueContext = Map.Map Name Type

prettyShow :: Pretty a => a -> String
prettyShow = show . pretty

instance Pretty Type where
  pretty (TVar a)     = text a
  pretty Int          = text "Int"
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ text "forall" <+> text a <> char '.' <+> pretty t
  pretty (Product ts) = parens $ sep $ intersperse (text " ,") (map pretty ts)
