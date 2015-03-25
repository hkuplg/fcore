{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  Core
Description :  Abstract syntax and pretty printer for Core.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Haoyuan Zhang <zhanghaoyuan00@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module Core
  ( Type(..)
  , Expr(..)
  , Alt(..)
  , TypeContext
  , ValueContext
  , Index
  , Constructor(..)
  , DataBind(..)
  , alphaEq
  , mapTVar
  , mapVar
  , fsubstTT
  , fsubstTE
  , fsubstEE
  , joinType
  , tVar
  , Core.forall
  , var
  , lam
  , fix
  , bLam
  , prettyType
  , prettyExpr
  , javaInt
  ) where

import qualified Src

import JavaUtils
import PrettyUtils

import Text.PrettyPrint.ANSI.Leijen
import qualified Language.Java.Pretty      (prettyPrint)

import           Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Type t
  = TVar Src.ReaderId t                -- a
  | JClass ClassName                   -- C
  | Fun (Type t) (Type t)              -- t1 -> t2
  | Forall Src.ReaderId (t -> Type t)  -- forall a. t
  | Product [Type t]                   -- (t1, ..., tn)
  | Unit
  | ListOf (Type t)
  | Datatype Src.ReaderId [Type t] [Src.ReaderId]

data Expr t e
  = Var Src.ReaderId e
  | Lit Src.Lit

  -- Binders we have: λ, fix, letrec, and Λ
  | Lam Src.ReaderId (Type t) (e -> Expr t e)
  | Fix Src.ReaderId Src.ReaderId
        (e -> e -> Expr t e)
        (Type t)  -- t1
        (Type t)  -- t
      -- fix x (x1 : t1) : t. e     Syntax in the tal-toplas paper
      -- fix (x : t1 -> t). \x1. e  Alternative syntax, which is arguably clear
      -- <name>: Fix funcName paraName func paraType returnType
  | Let Src.ReaderId (Expr t e) (e -> Expr t e)
  | LetRec [Src.ReaderId]       -- Names
           [Type t]             -- Signatures
           ([e] -> [Expr t e])  -- Bindings
           ([e] -> Expr t e)    -- Body
  | BLam Src.ReaderId (t -> Expr t e)

  | App  (Expr t e) (Expr t e)
  | TApp (Expr t e) (Type t)

  | If (Expr t e) (Expr t e) (Expr t e)
  | PrimOp (Expr t e) Src.Operator (Expr t e)
      -- SystemF extension from:
      -- https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf
      -- (no int restriction)

  | Tuple [Expr t e]     -- Tuple introduction
  | Proj Int (Expr t e)  -- Tuple elimination

  -- Java
  | JNew ClassName [Expr t e]
  | JMethod (Src.JCallee (Expr t e)) MethodName [Expr t e] ClassName
  | JField  (Src.JCallee (Expr t e)) FieldName ClassName
  | PolyList [Expr t e] (Type t)
  | JProxyCall (Expr t e) (Type t)

  | Seq [Expr t e]

  | Data Src.RecFlag [DataBind t] (Expr t e)
  | Constr (Constructor t) [Expr t e]
  | Case (Expr t e) [Alt t e]

data DataBind t = DataBind Src.ReaderId [Src.ReaderId] ([t] -> [Constructor t])

data Alt t e = ConstrAlt (Constructor t) [Src.ReaderId] ([e] -> Expr t e)
            -- | Default (Expr t e)

data Constructor t = Constructor {constrName :: Src.ReaderId, constrParams :: [Type t]}

type TypeContext t    = Set.Set t
type ValueContext t e = Map.Map e (Type t)

type Index = Int

alphaEq :: Int -> Type Index -> Type Index -> Bool
alphaEq _ (TVar _ a)   (TVar _ b)   = a == b
alphaEq _ (JClass c)   (JClass d)   = c == d
alphaEq i (Fun s1 s2)  (Fun t1 t2)  = alphaEq i s1 t1 && alphaEq i s2 t2
alphaEq i (Forall _ f) (Forall _ g) = alphaEq (succ i) (f i) (g i)
alphaEq i (Product ss) (Product ts) = length ss == length ts && uncurry (alphaEq i) `all` zip ss ts
alphaEq _  Unit         Unit        = True
alphaEq i (ListOf t1) (ListOf t2)   = alphaEq i t1 t2
alphaEq _  _            _           = False

mapTVar :: (Src.ReaderId -> t -> Type t) -> Type t -> Type t
mapTVar g (TVar n a)     = g n a
mapTVar _ (JClass c)     = JClass c
mapTVar g (Fun t1 t2)    = Fun (mapTVar g t1) (mapTVar g t2)
mapTVar g (Forall n f)   = Forall n (mapTVar g . f)
mapTVar g (Product ts)   = Product (map (mapTVar g) ts)
mapTVar _  Unit          = Unit
mapTVar g (Datatype n ts ns)  = Datatype n (map (mapTVar g) ts) ns
mapTVar g (ListOf t)     = ListOf (mapTVar g t)

mapVar :: (Src.ReaderId -> e -> Expr t e) -> (Type t -> Type t) -> Expr t e -> Expr t e
mapVar g _ (Var n a)                 = g n a
mapVar _ _ (Lit n)                   = Lit n
mapVar g h (Lam n t f)               = Lam n (h t) (mapVar g h . f)
mapVar g h (BLam n f)                = BLam n (mapVar g h . f)
mapVar g h (Fix n1 n2 f t1 t)        = Fix n1 n2 (\x x1 -> mapVar g h (f x x1)) (h t1) (h t)
mapVar g h (Let n b e)               = Let n (mapVar g h b) (mapVar g h . e)
mapVar g h (LetRec ns ts bs e)       = LetRec ns (map h ts) (map (mapVar g h) . bs) (mapVar g h . e)
mapVar g h (Data rec databinds e)    = Data rec (map mapDatabind databinds) (mapVar g h e)
    where mapDatabind (DataBind name params ctrs) = DataBind name params (map mapCtr. ctrs)
          mapCtr (Constructor n ts) = Constructor n (map h ts)
mapVar g h (Constr (Constructor n ts) es) = Constr c' (map (mapVar g h) es)
    where c' = Constructor n (map h ts)
mapVar g h (Case e alts)             = Case (mapVar g h e) (map mapAlt alts)
    where mapAlt (ConstrAlt (Constructor n ts) ns f) = ConstrAlt (Constructor n (map h ts)) ns ((mapVar g h) . f)
mapVar g h (App f e)                 = App (mapVar g h f) (mapVar g h e)
mapVar g h (TApp f t)                = TApp (mapVar g h f) (h t)
mapVar g h (If p b1 b2)              = If (mapVar g h p) (mapVar g h b1) (mapVar g h b2)
mapVar g h (PrimOp e1 op e2)         = PrimOp (mapVar g h e1) op (mapVar g h e2)
mapVar g h (Tuple es)                = Tuple (map (mapVar g h) es)
mapVar g h (Proj i e)                = Proj i (mapVar g h e)
mapVar g h (JNew c args)             = JNew c (map (mapVar g h) args)
mapVar g h (JMethod callee m args c) = JMethod (fmap (mapVar g h) callee) m (map (mapVar g h) args) c
mapVar g h (JField  callee f c)      = JField (fmap (mapVar g h) callee) f c
mapVar g h (Seq es)                  = Seq (map (mapVar g h) es)
mapVar g h (PolyList es t)           = PolyList (map (mapVar g h) es) (h t)
mapVar g h (JProxyCall jmethod t)    = JProxyCall (mapVar g h jmethod) (h t)

fsubstTT :: Eq a => a -> Type a -> Type a -> Type a
fsubstTT x r = mapTVar (\n a -> if a == x then r else TVar n a)

fsubstTE :: Eq t => t -> Type t -> Expr t e -> Expr t e
fsubstTE x r = mapVar Var (fsubstTT x r)

fsubstEE :: Eq a => a -> Expr t a -> Expr t a -> Expr t a
fsubstEE x r = mapVar (\n a -> if a == x then r else Var n a) id


joinType :: Type (Type t) -> Type t
joinType (TVar _ a)       = a
joinType (JClass c)       = JClass c
joinType (Fun t1 t2)      = Fun (joinType t1) (joinType t2)
joinType (Forall n g)     = Forall n (joinType . g . TVar "_") -- Right?
joinType (Product ts)     = Product (map joinType ts)
joinType  Unit            = Unit
joinType (Datatype n ts ns)  = Datatype n (map joinType ts) ns
joinType (ListOf t)       = ListOf (joinType t)

tVar :: t -> Type t
tVar = TVar "_"

forall :: (t -> Type t) -> Type t
forall f = Forall "_" f

var :: e -> Expr t e
var = Var "_"

lam :: Type t -> (e -> Expr t e) -> Expr t e
lam = Lam "_"

fix :: (e -> e -> Expr t e) -> Type t -> Type t -> Expr t e
fix = Fix "_" "_"

bLam :: (t -> Expr t e) -> Expr t e
bLam = BLam "_"

-- instance Show (Type Index) where
--   show = show . pretty

-- instance Pretty (Type Index) where
--   pretty = prettyType

prettyType :: Type Index -> Doc
prettyType = prettyType' basePrec 0

prettyType' :: Prec -> Index -> Type Index -> Doc

prettyType' _ _ (TVar n _)   = text n

prettyType' p i (Datatype n tvars _) = hsep $ text n : map (prettyType' p i) tvars

prettyType' p i (Fun t1 t2)  =
  parensIf p 2
    (prettyType' (2,PrecPlus) i t1 <+> arrow <+> prettyType' (2,PrecMinus) i t2)

prettyType' p i (Forall n f)   =
  parensIf p 1
    (PrettyUtils.forall <+> text n <> dot <+>
     prettyType' (1,PrecMinus) (succ i) (f i))

prettyType' _ i (Product ts) = parens $ hcat (intersperse comma (map (prettyType' basePrec i) ts))

prettyType' _ _  Unit = text "Unit"

prettyType' _ _ (JClass "java.lang.Integer")   = text "Int"
prettyType' _ _ (JClass "java.lang.String")    = text "String"
prettyType' _ _ (JClass "java.lang.Boolean")   = text "Bool"
prettyType' _ _ (JClass "java.lang.Character") = text "Char"
prettyType' _ _ (JClass c)                     = text c
prettyType' p i (ListOf t)                     = text "List" <+> prettyType' p i t

-- instance Show (Expr Index Index) where
--   show = show . pretty

-- instance Pretty (Expr Index Index) where
--   pretty = prettyExpr

prettyExpr :: Expr Index Index -> Doc
prettyExpr = prettyExpr' basePrec (0, 0)

prettyExpr' :: Prec -> (Index, Index) -> Expr Index Index -> Doc

prettyExpr' _ _ (Var n _) = text n

prettyExpr' p (i,j) (Lam n t f)
  = parensIf p 2 $ group $ hang 2 $
      lambda <+> parens (text n <+> colon <+> prettyType' basePrec i t) <+> text "->" <$>
      prettyExpr' (2,PrecMinus) (i, j + 1) (f j)

prettyExpr' p (i,j) (App e1 e2)
  = parensIf p 4 $
      group $ hang 2 $ prettyExpr' (4,PrecMinus) (i,j) e1 <$> prettyExpr' (4,PrecPlus) (i,j) e2

prettyExpr' p (i,j) (BLam n f) =
  parensIf p 2
    (biglambda <+> text n <+> text "->" <+>
     prettyExpr' (2,PrecMinus) (succ i, j) (f i))

prettyExpr' p (i,j) (TApp e t) =
  parensIf p 4
    (group $ hang 2 $ prettyExpr' (4,PrecMinus) (i,j) e <$> brackets (prettyType' basePrec i t))

prettyExpr' _ _ (Lit (Src.Int n))    = integer n
prettyExpr' _ _ (Lit (Src.String s)) = dquotes (string s)
prettyExpr' _ _ (Lit (Src.Bool b))   = bool b
prettyExpr' _ _ (Lit (Src.Char c))   = char c
prettyExpr' _ _ (Lit Src.UnitLit)    = unit

prettyExpr' p (i,j) (If e1 e2 e3)
  = parensIf p prec
      (hang 3 (text "if"   <+> prettyExpr' (prec,PrecMinus) (i,j) e1 <+>
               text "then" <+> prettyExpr' (prec,PrecMinus) (i,j) e2 <+>
               text "else" <+> prettyExpr' (prec,PrecMinus) (i,j) e3))
  where prec = 3

prettyExpr' p (i,j) (PrimOp e1 op e2)
  = parens (prettyExpr' p (i,j) e1 <+> pretty_op <+> prettyExpr' p (i,j) e2)
  where
    pretty_op = text (Language.Java.Pretty.prettyPrint java_op)
    java_op   = case op of
                  Src.Arith   op' -> op'
                  Src.Compare op' -> op'
                  Src.Logic   op' -> op'

prettyExpr' _ (i,j) (Tuple es) = tupled (map (prettyExpr' basePrec (i,j)) es)

prettyExpr' p i (Proj n e) =
  parensIf p 5
    (prettyExpr' (5,PrecMinus) i e <> dot <> char '_' <> int n)

prettyExpr' _ (i,j) (JNew c args) =
  parens (text "new" <+> text c <> tupled (map (prettyExpr' basePrec (i,j)) args))

prettyExpr' _ i (JMethod name m args _) = methodStr name <> dot <> text m <> tupled (map (prettyExpr' basePrec i) args)
  where
    methodStr (Src.Static x) = text x
    methodStr (Src.NonStatic x) = prettyExpr' (6,PrecMinus) i x

prettyExpr' _ i (JField name f _) = fieldStr name <> dot <> text f
  where
    fieldStr (Src.Static x) = text x
    fieldStr (Src.NonStatic x) = prettyExpr' (6,PrecMinus) i x

prettyExpr' p i (PolyList es t) = brackets. hcat . intersperse comma . map (prettyExpr' p i ) $ es
prettyExpr' p i (JProxyCall jmethod t) = prettyExpr' p i jmethod

prettyExpr' p (i,j) (Seq es) = semiBraces (map (prettyExpr' p (i,j)) es)

prettyExpr' p (i,j) (Fix n1 n2 f t1 t)
  = parens $ group $ hang 2 $
      text "fix" <+> text n1 <+>
      parens (text n2 <+> colon <+> prettyType' p i t1) <+>
      colon <+> prettyType' p i t <> dot <$>
      prettyExpr' p (i, j + 2) (f j (j + 1))

prettyExpr' _ (i,j) (Let n b e) =
  text "let" <+> text n <+> equals <+> prettyExpr' basePrec (i, j + 1) b <$> text "in" <$>
  prettyExpr' basePrec (i, j + 1) (e j)

prettyExpr' p (i,j) (LetRec names sigs binds body)
  = text "let" <+> text "rec" <$>
    vcat (intersperse (text "and") (map (indent 2) pretty_binds)) <$>
    text "in" <$>
    pretty_body
  where
    n   = length sigs
    ids = [i..(i+n-1)]
    pretty_ids   = map text names
    pretty_sigs  = map (prettyType' p i) sigs
    pretty_defs  = map (prettyExpr' p (i, j + n)) (binds ids)
    pretty_binds = zipWith3 (\pretty_id pretty_sig pretty_def ->
                  pretty_id <+> colon <+> pretty_sig <$> indent 2 (equals <+> pretty_def))
                  pretty_ids pretty_sigs pretty_defs
    pretty_body  = prettyExpr' p (i, j + n) (body ids)

prettyExpr' p (i,j) (Data recflag databinds e) =
  text "data" <+> (pretty recflag) <+> (align .vsep) (map prettyDatabind databinds) <$> prettyExpr' p (i,j) e
    where prettyCtr i' (Constructor ctrName ctrParams) = (text ctrName) <+> (hsep. map (prettyType' p i') $ ctrParams)
          prettyDatabind (DataBind n tvars cons) = hsep (map text $ n:tvars) <+> align
                   (equals <+> intersperseBar (map (prettyCtr (i+ (length tvars)))$ cons [i..(i-1+(length tvars))]) <$$> semi)

prettyExpr' p (i,j) (Constr c es)            = parens $ hsep $ text (constrName c) : map (prettyExpr' p (i,j)) es

prettyExpr' p (i,j) (Case e alts) =
    hang 2 $ text "case" <+> prettyExpr' p (i,j) e <+> text "of" <$> text " " <+> Src.intersperseBar (map pretty_alt alts)
    where pretty_alt (ConstrAlt c ns es) =
              let n = length ns
                  ids = [j..j+n-1]
              in hsep (text (constrName c) : (map prettyVar ids)) <+> arrow <+> prettyExpr' p (i, j+n) (es ids)

javaInt :: Type t
javaInt = JClass "java.lang.Integer"
