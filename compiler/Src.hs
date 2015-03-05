{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  Src
Description :  Abstract syntax and pretty printer for the source language.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable
-}

{- References for syntax:
   http://www.haskell.org/onlinereport/exps.html
   http://www.haskell.org/onlinereport/decls.html
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html -}

module Src
  ( Module(..), ReaderModule
  , Kind(..)
  , Type(..), ReaderType
  , Expr(..), ReaderExpr, CheckedExpr
  , Constructor(..), Alt(..)
  , Bind(..), ReaderBind
  , RecFlag(..), Lit(..), Operator(..), UnitPossibility(..), JCallee(..), JVMType(..), Label
  , Name, ReaderId, CheckedId
  , TypeValue(..), TypeContext, ValueContext
  , expandType, alphaEq, subtype
  , deThunkOnce
  , recordFields
  , freeTVars
  , fsubstTT
  , wrap
  , opPrec
  , intersperseBar
  ) where

import Config
import JavaUtils
import PrettyUtils
import Panic


import qualified Language.Java.Syntax as J (Op(..))
-- import qualified Language.Java.Pretty as P
import Text.PrettyPrint.ANSI.Leijen

import Control.Arrow (second)

import Data.Data
import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Names and identifiers.
type Name      = String
type ReaderId  = Name
type CheckedId = (ReaderId, Type)
type Label      = Name
-- type ConstrName = Name

-- Modules.
data Module id ty = Module id [Bind id ty] deriving (Eq, Show)
type ReaderModule = Module ReaderId Type

-- Kinds k := * | k -> k
data Kind = Star | KArrow Kind Kind deriving (Eq, Show)

-- Types.
data Type
  = TVar Name
  | JType JVMType -- JClass ClassName
  | Unit
  | Fun Type Type
  | Forall Name Type
  | Product [Type]
  -- Extensions
  | And Type Type
  | RecordType [(Label, Type)]
  | Thunk Type

  -- Type synonyms
  | OpAbs Name Type -- Type-level abstraction: "type T A = t" becomes "type T = \A. t", and "\A. t" is the abstraction.
  | OpApp Type Type -- Type-level application: t1 t2

  | ListOf Type
  | Datatype Name [Type] [Name]

  -- Warning: If you ever add a case to this, you MUST also define the binary
  -- relations on your new case. Namely, add cases for your data constructor in
  -- `alphaEq` and `subtype` below.
  deriving (Eq, Show, Data, Typeable)

type ReaderType = Type

data JVMType = JClass ClassName | JPrim String deriving (Eq, Show, Data, Typeable)

-- Expressions.
data Expr id ty
  = Var id                                    -- Variable
  | Lit Lit                                   -- Literals
  | Lam (Name, ty) (Expr id ty)             -- Lambda
  | App  (Expr id ty) (Expr id ty)            -- Application
  | BLam Name (Expr id ty)                    -- Big lambda
  | TApp (Expr id ty) ty                    -- Type application
  | Tuple [Expr id ty]                        -- Tuples
  | Proj (Expr id ty) Int                     -- Tuple projection
  | PrimOp (Expr id ty) Operator (Expr id ty) -- Primitive operation
  | If (Expr id ty) (Expr id ty) (Expr id ty) -- If expression
  | Let RecFlag [Bind id ty] (Expr id ty)     -- Let (rec) ... (and) ... in ...
  | LetOut                                    -- Post typecheck only
      RecFlag
      [(Name, Type, Expr (Name,Type) Type)]
      (Expr (Name,Type) Type)

  | Dot (Expr id ty) Name (Maybe ([Expr id ty], UnitPossibility))
  -- The flag `UnitPossibility` is only used when length of the argument list is
  -- 0, to distinguish the different possible interpretations of `e.x ( )` and
  -- `e.x ()` -- the latter can be an application (of unit literal to a record
  -- elim), while the former cannot.

  | JNew ClassName [Expr id ty]
  | JMethod (JCallee (Expr id ty)) MethodName [Expr id ty] ClassName
  | JField  (JCallee (Expr id ty)) FieldName            ClassName
  | Seq [Expr id ty]
  | PolyList [Expr id ty] ty
  | Merge (Expr id ty) (Expr id ty)
  | RecordCon [(Label, Expr id ty)]
  | RecordProj (Expr id ty) Label
  | RecordUpdate (Expr id ty) [(Label, Expr id ty)]
  | LetModule (Module id ty) (Expr id ty)
  | ModuleAccess Name Name
  | Type -- type T A1 .. An = t in e
      Name         -- T         -- Name of type constructor
      [Name]       -- A1 ... An -- Type parameters
      Type   -- t         -- RHS of the equal sign
      (Expr id ty) -- e         -- The rest of the expression
  | Data Name [Name] [Constructor] (Expr id ty)
  | Case (Expr id ty) [Alt id ty]
  | ConstrTemp Name
  | Constr Constructor [Expr id ty] -- post typecheck only
  | JProxyCall (Expr id ty) ty
  deriving (Eq, Show)

data Constructor = Constructor {constrName :: Name, constrParams :: [Type]}
                   deriving (Eq, Show)

data Alt id ty = ConstrAlt Constructor [Name] (Expr id ty)
            -- | Default (Expr id)
              deriving (Eq, Show)

-- type RdrExpr = Expr Name
type ReaderExpr  = Expr ReaderId  Type
type CheckedExpr = Expr CheckedId Type
-- type TcExpr  = Expr TcId
-- type TcBinds = [(Name, Type, Expr TcId)] -- f1 : t1 = e1 and ... and fn : tn = en

data Lit -- Data constructor names match Haskell types
  = Int Integer
  | String String
  | Bool Bool
  | Char Char
  | UnitLit
  deriving (Eq, Show)

data Operator = Arith J.Op | Compare J.Op | Logic J.Op deriving (Eq, Show)

data Bind id ty = Bind
  { bindId       :: id             -- Identifier
  , bindTyParams :: [Name]         -- Type arguments
  , bindParams   :: [(Name, Type)] -- Arguments, each annotated with a type
  , bindRhs      :: Expr id ty     -- RHS to the "="
  , bindRhsTyAscription :: Maybe Type  -- Type annotation for the RHS
  } deriving (Eq, Show)

type ReaderBind = Bind Name Type

data RecFlag = Rec | NonRec deriving (Eq, Show)
data UnitPossibility = UnitPossible | UnitImpossible deriving (Eq, Show)

data JCallee e = Static ClassName | NonStatic e deriving (Eq, Show)

instance Functor JCallee where
  fmap _ (Static c)    = Static c
  fmap f (NonStatic e) = NonStatic (f e)


-- Type and value contexts

-- `TypeValue` is what's put inside a type context.
data TypeValue
  = TerminalType -- Terminal types, e.g., the `a` of `forall a. `
  | NonTerminalType Type
    -- Non-terminal types, i.e. type synoyms. `Type` holds the RHS to the
    -- equal sign of type synonym definitions.

type TypeContext  = Map.Map ReaderId (Kind, TypeValue) -- Delta
type ValueContext = Map.Map ReaderId Type              -- Gamma


-- | Recursively expand all type synonyms. The given type must be well-kinded.
-- Used in `alphaEq` and `subtype`.
expandType :: TypeContext -> Type -> Type

-- Interesting cases:
expandType d (TVar a)
  = case Map.lookup a d of
      Nothing                       -> prettyPanic "expandType" (pretty (TVar a))
      Just (_, TerminalType)        -> TVar a
      Just (_, NonTerminalType def) -> expandType d def
expandType d (OpAbs x t) = OpAbs x (expandType (Map.insert x (Star, TerminalType) d) t)
expandType d (OpApp t1 t2)
  = let t1' = expandType d t1
        t2' = expandType d t2
    in
    case t1' of
      OpAbs x t -> fsubstTT (x,t2') t

-- Uninteresting cases:
expandType _ (JType t)    = JType t
expandType _ Unit         = Unit
expandType d (Fun t1 t2)  = Fun (expandType d t1) (expandType d t2)
expandType d (Forall a t) = Forall a (expandType (Map.insert a (Star, TerminalType) d) t)
expandType d (Product ts) = Product (map (expandType d) ts)
expandType d (RecordType fs)  = RecordType (map (second (expandType d)) fs)
expandType d (ListOf t)   = ListOf (expandType d t)
expandType d (And t1 t2)  = And (expandType d t1) (expandType d t2)
expandType d (Thunk t)    = Thunk (expandType d t)
expandType d (Datatype n ts ns) = Datatype n (map (expandType d) ts) ns

-- Type equivalence(s) and subtyping

deThunkOnce :: Type -> Type
deThunkOnce (Thunk t) = t
deThunkOnce t         = t

-- | Alpha equivalence.
alphaEq :: TypeContext -> Type -> Type -> Bool
alphaEq d t1 t2 = subtype d t1' t2' && subtype d t2' t1'
  where
    t1' = expandType d t1
    t2' = expandType d t2

-- | Alpha equivalance of two *expanded* types.
alphaEqS :: Type -> Type -> Bool
alphaEqS (TVar a) (TVar b)             = a == b

-- The ground for this? Can you provide an example?
-- alphaEqS (JType (JPrim "char")) (JType (JClass "java.lang.Character")) = True
-- alphaEqS (JType (JClass "java.lang.Character")) (JType (JPrim "char")) = True

alphaEqS (JType c)      (JType d)      = c == d
alphaEqS (Fun t1 t2)    (Fun t3 t4)    = alphaEqS t1 t3 && alphaEqS t2 t4
alphaEqS (Forall a1 t1) (Forall a2 t2) = alphaEqS (fsubstTT (a2, TVar a1) t2) t1
alphaEqS (Product ts1)  (Product ts2)  = length ts1 == length ts2 && uncurry alphaEqS `all` zip ts1 ts2
alphaEqS (RecordType [(l1,t1)]) (RecordType [(l2,t2)]) = l1 == l2 && alphaEqS t1 t2
alphaEqS (RecordType fs1)   (RecordType fs2)           = alphaEqS (desugarMultiRecordType fs1) (desugarMultiRecordType fs2)
alphaEqS (ListOf t1)    (ListOf t2)    = alphaEqS t1 t2
alphaEqS (And t1 t2)    (And t3 t4)    = alphaEqS t1 t3 && alphaEqS t2 t4
alphaEqS Unit           Unit           = True
alphaEqS (Thunk t1)     t2             = alphaEqS t1 t2
alphaEqS t1             (Thunk t2)     = alphaEqS t1 t2
alphaEqS (Datatype n1 ts1 m1) (Datatype n2 ts2 m2) =
    n1 == n2 && m1 == m2 && length ts1 == length ts2 && (uncurry alphaEqS `all` zip ts1 ts2)
alphaEqS t1             t2             = False `panicOnSameDataCons` ("Src.alphaEqS", t1, t2)


-- | Subtyping.
subtype :: TypeContext -> Type -> Type -> Bool
subtype d t1 t2 = subtypeS (expandType d t1) (expandType d t2)

-- | Subtyping of two *expanded* types.
subtypeS :: Type -> Type -> Bool
subtypeS t1             (Thunk t2)             = subtypeS t1 t2
subtypeS (Thunk t1)     t2                     = subtypeS t1 t2
subtypeS (TVar a)       (TVar b)               = a == b
subtypeS (JType c)      (JType d)              = c == d
-- The subtypeS here shouldn't be aware of the subtyping relations in the Java world.
subtypeS (Fun t1 t2)    (Fun t3 t4)            = subtypeS t3 t1 && subtypeS t2 t4
subtypeS (Forall a1 t1) (Forall a2 t2)         = subtypeS (fsubstTT (a1,TVar a2) t1) t2
subtypeS (Product ts1)  (Product ts2)          = length ts1 == length ts2 && uncurry subtypeS `all` zip ts1 ts2
subtypeS (RecordType [(l1,t1)]) (RecordType [(l2,t2)]) = l1 == l2 && subtypeS t1 t2
subtypeS (RecordType fs1)   (RecordType fs2)           = subtypeS (desugarMultiRecordType fs1) (desugarMultiRecordType fs2)
subtypeS (ListOf t1)    (ListOf t2)            = subtypeS t1 t2  -- List :: * -> * is covariant
-- The order is significant for the two `And` cases below.
subtypeS t1             (And t2 t3) = subtypeS t1 t2 && subtypeS t1 t3
subtypeS (And t1 t2)    t3          = subtypeS t1 t3 || subtypeS t2 t3
subtypeS Unit           Unit        = True
subtypeS (Datatype n1 ts1 m1) (Datatype n2 ts2 m2) =
    n1 == n2 && m1 == m2 && length ts1 == length ts2 && uncurry subtypeS `all` zip ts1 ts2
subtypeS t1             t2          = False `panicOnSameDataCons` ("Src.subtypeS", t1, t2)

-- Records

-- TODO: refactor the following two functions

desugarRecordType :: Type -> Type
desugarRecordType (RecordType [(l,t)]) = RecordType [(l,t)]
desugarRecordType (RecordType fs)      = desugarMultiRecordType fs
desugarRecordType (And t1 t2)      = And (desugarRecordType t1) (desugarRecordType t2)
desugarRecordType t                = t
-- FIXME: incomplete cases



desugarMultiRecordType :: [(Label,Type)] -> Type
desugarMultiRecordType []         = panic "Src.desugarMultiRecordTy"
desugarMultiRecordType [(l,t)]    = RecordType [(l,t)]
desugarMultiRecordType ((l,t):fs) = RecordType [(l,t)] `And` desugarMultiRecordType fs

-- | Returns the record fields of a type. Note that a type does not have to be a
-- record by itself in order for it to have fields. (See the second example
-- below.)
-- Examples (in pseudo-code):
--   recordFields(String) = {}
--   recordFields(String&{name:String, age:Int}) = {"name" => String, "age" => Int}
recordFields :: Type -> Map.Map Label Type
recordFields (RecordType fs) =
  case intersectionBias of
    -- `Map.fromList` is right-biased.
    -- For example:
    --   ghci> Map.fromList [(1,"one"),(1,"yat")]
    --   fromList [(1,"yat")]
    LeftBiased  -> Map.fromList (reverse fs)
    RightBiased -> Map.fromList fs
recordFields (And t1 t2) =
  case intersectionBias of
    -- But `Map.union` is left-biased.
    -- For example:
    --   ghci> Map.fromList [(1,"one")] `Map.union` Map.fromList [(1,"yat")]
    --   fromList [(1,"one")]
    LeftBiased  -> recordFields t1 `Map.union` recordFields t2
    RightBiased -> recordFields t2 `Map.union` recordFields t1
recordFields (Thunk t) = recordFields t
recordFields _         = Map.empty

-- Free variable substitution

fsubstTT :: (Name, Type) -> Type -> Type
fsubstTT (x,r) (TVar a)
  | a == x                     = r
  | otherwise                  = TVar a
-- fsubstTT (_,_) (JClass c )     = JClass c
fsubstTT (_,_) (JType c)       = JType c
fsubstTT (x,r) (Fun t1 t2)     = Fun (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (Product ts)    = Product (map (fsubstTT (x,r)) ts)
fsubstTT (x,r) (Forall a t)
  | a == x                     = Forall a t
  | a `Set.member` freeTVars r = Forall a t -- The freshness condition, crucial!
  | otherwise                  = Forall a (fsubstTT (x,r) t)
fsubstTT (x,r) (ListOf a)      = ListOf (fsubstTT (x,r) a)
fsubstTT (_,_) Unit            = Unit
fsubstTT (x,r) (RecordType fs)     = RecordType (map (second (fsubstTT (x,r))) fs)
fsubstTT (x,r) (And t1 t2)     = And (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (Thunk t1)      = Thunk (fsubstTT (x,r) t1)
fsubstTT (x,r) (OpAbs a t)
  | a == x                     = OpAbs a t
  | a `Set.member` freeTVars r = OpAbs a t -- The freshness condition, crucial!
  | otherwise                  = OpAbs a (fsubstTT (x,r) t)
fsubstTT (x,r) (OpApp t1 t2)   = OpApp (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (Datatype n ts ns) = Datatype n (map (fsubstTT (x,r)) ts) ns

freeTVars :: Type -> Set.Set Name
freeTVars (TVar x)     = Set.singleton x
-- freeTVars (JClass _)    = Set.empty
freeTVars (JType _)    = Set.empty
freeTVars Unit         = Set.empty
freeTVars (Fun t1 t2)  = freeTVars t1 `Set.union` freeTVars t2
freeTVars (Forall a t) = Set.delete a (freeTVars t)
freeTVars (Product ts) = Set.unions (map freeTVars ts)
freeTVars (RecordType fs)  = Set.unions (map (\(_l,t) -> freeTVars t) fs)
freeTVars (ListOf t)   = freeTVars t
freeTVars (And t1 t2)  = Set.union (freeTVars t1) (freeTVars t2)
freeTVars (Thunk t)    = freeTVars t
freeTVars (OpAbs _ t)  = freeTVars t
freeTVars (OpApp t1 t2) = Set.union (freeTVars t1) (freeTVars t2)
freeTVars (Datatype _ ts _) = Set.unions (map freeTVars ts)

-- Pretty printers

instance Pretty Kind where
  pretty Star           = char '*'
  pretty (KArrow k1 k2) = parens (pretty k1 <+> text "=>" <+> pretty k2)

instance Pretty Type where
  pretty (TVar a)     = text a
  pretty (JType (JClass "java.lang.Integer"))   = text "Int"
  pretty (JType (JClass "java.lang.String"))    = text "String"
  pretty (JType (JClass "java.lang.Boolean"))   = text "Bool"
  pretty (JType (JClass "java.lang.Character")) = text "Char"
  pretty (JType (JClass c))   = text c
  pretty (JType (JPrim c))   = text c
  pretty Unit         = text "Unit"
  pretty (Fun t1 t2)  = parens $ pretty t1 <+> text "->" <+> pretty t2
  pretty (Forall a t) = parens $ forall <+> text a <> dot <+> pretty t
  pretty (Product ts) = lparen <> hcat (intersperse comma (map pretty ts)) <> rparen
  pretty (And t1 t2)  = pretty t1 <> text "&" <> pretty t2
  pretty (RecordType fs)  = lbrace <> hcat (intersperse comma (map (\(l,t) -> text l <> colon <> pretty t) fs)) <> rbrace
  pretty (Thunk t)    = squote <> parens (pretty t)
  pretty (OpAbs x t)  = backslash <> text x <> dot <+> pretty t
  pretty (OpApp t1 t2) = parens (pretty t1 <+> pretty t2)
  pretty (ListOf a)   = brackets $ pretty a
  pretty (Datatype n ts _) = hsep (text n : map pretty ts)

instance (Show id, Pretty id, Show ty, Pretty ty) => Pretty (Expr id ty) where
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
  pretty (Tuple es) = lparen <> hcat (intersperse comma (map pretty es)) <> rparen
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
  pretty (JNew c args)  = text "new" <+> text c <> tupled (map pretty args)
  pretty (JMethod e m args _) = case e of (Static c)     -> pretty c  <> dot <> text m <> tupled (map pretty args)
                                          (NonStatic e') -> pretty e' <> dot <> text m <> tupled (map pretty args)
  pretty (JField e f _) = case e of (Static c)     -> pretty c  <> dot <> text f
                                    (NonStatic e') -> pretty e' <> dot <> text f
  pretty (PolyList l _)    = brackets . hcat . intersperse comma $ map pretty l
  pretty (JProxyCall jmethod t) = pretty jmethod
  pretty (Merge e1 e2)  = parens (pretty e1 <+> text ",," <+> pretty e2)
  pretty (RecordCon fs) = lbrace <> hcat (intersperse comma (map (\(l,t) -> text l <> equals <> pretty t) fs)) <> rbrace
  pretty (Data n tvars cons e) = text "data" <+> hsep (map text $ n:tvars) <+> align (equals <+> intersperseBar (map pretty cons) <$$> semi) <$>
                           pretty e

  pretty (Case e alts) = hang 2 (text "case" <+> pretty e <+> text "of" <$> text " " <+> intersperseBar (map pretty alts))
  pretty (Constr c es) = parens $ hsep $ text (constrName c) : map pretty es
  pretty e = text (show e)

instance (Show id, Pretty id, Show ty, Pretty ty) => Pretty (Bind id ty) where
  pretty Bind{..} =
    pretty bindId <+>
    hsep (map pretty bindTyParams) <+>
    hsep (map (\(x,t) -> parens (pretty x <+> colon <+> pretty t)) bindParams) <+>
    case bindRhsTyAscription of { Nothing -> empty; Just t -> colon <+> pretty t } <+>
    equals <+>
    pretty bindRhs

instance Pretty RecFlag where
  pretty Rec    = text "rec"
  pretty NonRec = empty

instance Pretty Constructor where
    pretty (Constructor n ts) = hsep $ text n : map pretty ts

instance (Show id, Pretty id, Show ty, Pretty ty) => Pretty (Alt id ty) where
    pretty (ConstrAlt c ns e2) = hsep (text (constrName c) : map text ns) <+> arrow <+> pretty e2
    -- pretty (Default e) = text "_" <+> arrow <+> pretty e

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
