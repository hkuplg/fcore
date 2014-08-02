{- References for syntax:
   http://www.haskell.org/onlinereport/exps.html
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html -}

{-# LANGUAGE RecordWildCards #-}

module ESF.Syntax
  ( Type(..)
  , eqType
  , Lit(..)
  , Expr(..)
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

data Type
  = TyVar String
  | Int
  | Fun Type Type
  | Forall String Type
  | Product [Type]
  deriving (Eq, Show)

eqType :: Type -> Type -> Bool
eqType = (==)

data Lit
  = Integer Integer -- later maybe Bool | Char
  deriving (Eq, Show)

data Expr e
  = Var e                          -- Variable
  | Lit Lit                        -- Literals
  | Lam (e, Type) (Expr e)         -- Lambda abstraction
  | App  (Expr e) (Expr e)         -- Application
  | BLam String (Expr e)             -- Type lambda abstraction
  | TApp (Expr e) Type             -- Type application
  | Tuple [Expr e]                 -- Tuples
  | Proj (Expr e) Int              -- Tuple projection
  | PrimOp (Expr e) J.Op (Expr e)  -- Primitive operation
  | If0 (Expr e) (Expr e) (Expr e) -- If expression
  | Let RecFlag [Bind e] (Expr e)  -- Let (rec) ... (and) ... in ...
  deriving (Eq, Show)

-- f A1 ... An (x : T1) ... (x : Tn) : T = e
data Bind e = Bind
  { bindId       :: e           -- Identifier
  , bindTargs    :: [String]      -- Type arguments
  , bindArgs     :: [(e, Type)] -- Arguments, each annotated with a type
  , bindRhs      :: Expr e      -- RHS to the "="
  , bindRhsAnnot :: Maybe Type  -- Type of the RHS
  } deriving (Eq, Show)

data RecFlag = Rec | NonRec deriving (Eq, Show)

type TypeContext  = Set.Set String
type ValueContext = Map.Map String Type

instance Pretty Type where
  pretty (TyVar a)    = text a
  pretty Int          = text "Int"
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ text "forall" <+> text a <> dot <+> pretty t
  pretty (Product ts) = tupled (map pretty ts)

instance Pretty e => Pretty (Expr e) where
  pretty (Var x) = pretty x
  pretty (Lit (Integer n)) = integer n
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

instance Pretty e => Pretty (Bind e) where
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

invariantFailed :: String -> String -> a
invariantFailed location msg = error ("Invariant failed in " ++ location ++ ": " ++ msg)
