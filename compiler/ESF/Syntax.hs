{- References for syntax:
   http://www.haskell.org/onlinereport/exps.html
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html -}

{-# LANGUAGE RecordWildCards #-}

module ESF.Syntax
  ( Type(..)
  , alphaEqTy
  , substFreeTyVars
  , freeTyVars
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
  | JClass String
  deriving (Show)

data Lit
  = Integer Integer -- later maybe Bool | Char
  deriving (Show)

data Expr e
  = Var e                          -- Variable
  | Lit Lit                        -- Literals
  | Lam (String, Type) (Expr e)    -- Lambda abstraction
  | App  (Expr e) (Expr e)         -- Application
  | BLam String (Expr e)             -- Type lambda abstraction
  | TApp (Expr e) Type             -- Type application
  | Tuple [Expr e]                 -- Tuples
  | Proj (Expr e) Int              -- Tuple projection
  | PrimOp (Expr e) J.Op (Expr e)  -- Primitive operation
  | If0 (Expr e) (Expr e) (Expr e) -- If expression
  | Let RecFlag [Bind e] (Expr e)  -- Let (rec) ... (and) ... in ...
  | JNewObj String [Expr e]          -- New Java object
  | JMethod (Expr e) String [Expr e] -- Java method call
  deriving (Show)

-- f A1 ... An (x : T1) ... (x : Tn) : T = e
data Bind e = Bind
  { bindId       :: e                -- Identifier
  , bindTargs    :: [String]         -- Type arguments
  , bindArgs     :: [(String, Type)] -- Arguments, each annotated with a type
  , bindRhs      :: Expr e           -- RHS to the "="
  , bindRhsAnnot :: Maybe Type       -- Type of the RHS
  } deriving (Show)

data RecFlag = Rec | NonRec deriving (Show)

type TypeContext  = Set.Set String
type ValueContext = Map.Map String Type

alphaEqTy :: Type -> Type -> Bool
alphaEqTy (TyVar a)      (TyVar b)      = a == b
alphaEqTy  Int            Int           = True
alphaEqTy (Fun t1 t2)    (Fun t3 t4)    = t1 `alphaEqTy` t3 && t2 `alphaEqTy` t4
alphaEqTy (Product ts1)  (Product ts2)  = length ts1 == length ts2 &&
                                            (\(t1,t2) -> t1 `alphaEqTy` t2)
                                              `all` zip ts1 ts2
alphaEqTy (Forall a1 t1) (Forall a2 t2) = substFreeTyVars (a2, TyVar a1) t2 `alphaEqTy` t1
alphaEqTy  _              _             = False

-- Capture-avoiding substitution
-- http://en.wikipedia.org/wiki/Lambda_calculus#Capture-avoiding_substitutions
substFreeTyVars :: (String, Type) -> Type -> Type
substFreeTyVars (x, r) = go
  where
    go (TyVar a)
      | a == x      = r
      | otherwise   = TyVar a
    go Int          = Int
    go (JClass c )  = JClass c
    go (Fun t1 t2)  = Fun (go t1) (go t2)
    go (Product ts) = Product (map go ts)
    go (Forall a t)
      | a == x                      = Forall a t
      | a `Set.member` freeTyVars r = Forall a t -- The freshness condition, crucial!
      | otherwise                   = Forall a (go t)

freeTyVars :: Type -> Set.Set String
freeTyVars (TyVar x)    = Set.singleton x
freeTyVars  Int         = Set.empty
freeTyVars (JClass _)   = Set.empty
freeTyVars (Forall a t) = Set.delete a (freeTyVars t)
freeTyVars (Fun t1 t2)  = freeTyVars t1 `Set.union` freeTyVars t2
freeTyVars (Product ts) = Set.unions (map freeTyVars ts)

instance Pretty Type where
  pretty (TyVar a)    = text a
  pretty Int          = text "Int"
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ text "forall" <+> text a <> dot <+> pretty t
  pretty (Product ts) = tupled (map pretty ts)
  pretty (JClass c)   = text c

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
  pretty (JNewObj c args)  = text "new" <+> text c <> tupled (map pretty args)
  pretty (JMethod e m args) = pretty e <> dot <> text m <> tupled (map pretty args)

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
