{-# LANGUAGE FlexibleInstances, RankNTypes #-}

{- |
Module      :  SystemFI
Description :  Abstract syntax and pretty printer for SystemFI.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Haoyuan Zhang <zhanghaoyuan00@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module SystemFI
  ( Type(..)
  , Expr(..)
  , FExp(..)
  , Constructor(..)
  , Alt(..)
  , DataBind(..)
  , Definition(..)
--, TypeContext
--, ValueContext
--, Index
--, alphaEq
  , mapTVar
--, mapVar
  , fsubstTT
  , fsubstTE
  , fsubstEE
  , joinType
  , prettyType
  , prettyExpr
  ) where

import           JavaUtils
import           PrettyUtils
import qualified Src

import           Control.Arrow (second)
import           Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Java.Pretty (prettyPrint)
import           Prelude hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

data Type t
  = TVar Src.ReadId t                 -- a
  | JClass ClassName                    -- C
  | Fun (Type t) (Type t)               -- t1 -> t2
  | Forall Src.ReadId (t -> Type t)   -- forall a. t
  | Product [Type t]                    -- (t1, ..., tn)
  | Unit
  -- Extensions
  | And (Type t) (Type t)               -- t1 & t2
  | RecordType (Src.Label, Type t)
  | Datatype Src.ReadId [Type t] [Src.ReadId]
    -- Warning: If you ever add a case to this, you *must* also define the
    -- binary relations on your new case. Namely, add cases for your data
    -- constructor in `alphaEq' (below) and `coerce' (in Simplify.hs). Consult
    -- George if you're not sure.

data Definition t e = Def Src.Name (Src.Type, Type t) (Expr t e) (e -> Definition t e)
                    | DefRec [Src.Name] [(Src.Type, Type t)] ([e] -> [Expr t e]) ([e] -> Definition t e)
                    | Null

data Expr t e
  = Var Src.ReadId e
  | Lit Src.Lit

  -- Binders we have: λ, fix, letrec, and Λ
  | Lam Src.ReadId (Type t) (e -> Expr t e)
  | Fix Src.ReadId Src.ReadId
        (e -> e -> Expr t e)
        (Type t)  -- t1
        (Type t)  -- t
      -- fix x (x1 : t1) : t. e     Syntax in the tal-toplas paper
      -- fix (x : t1 -> t). \x1. e  Alternative syntax, which is arguably clear
      -- <name>: Fix funcName paraName func paraType returnType
  | Let Src.ReadId (Expr t e) (e -> Expr t e)
  | LetRec [Src.ReadId]           -- Names
           [Type t]                 -- Signatures
           ([e] -> [Expr t e])      -- Bindings
           ([e] -> Expr t e)        -- Body
  | BLam Src.ReadId (t -> Expr t e)

  | App  (Expr t e) (Expr t e)
  | TApp (Expr t e) (Type t)

  | If (Expr t e) (Expr t e) (Expr t e)
  | PrimOp (Expr t e) Src.Operator (Expr t e)
      -- SystemF extension from:
      -- https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf
      -- (no int restriction)

  | Tuple [Expr t e]     -- Tuple introduction
  | Proj Int (Expr t e)  -- Tuple elimination

  -- Module
  | Module (Maybe Src.PackageName) (Definition t e)

  -- Java
  | JNew ClassName [Expr t e]
  | JMethod (Src.JReceiver (Expr t e)) MethodName [Expr t e] ClassName
  | JField  (Src.JReceiver (Expr t e)) FieldName (Type t)

  | Seq [Expr t e]

  | Merge (Expr t e) (Expr t e)  -- e1 ,, e2
  | RecordCon    (Src.Label, Expr t e)
  | RecordProj   (Expr t e) Src.Label
  | RecordUpdate (Expr t e) (Src.Label, Expr t e)

  | Data Src.RecFlag [DataBind t] (Expr t e)
  | ConstrOut (Constructor t) [Expr t e]
  | Case (Expr t e) [Alt t e]

  | Error (Type t) (Expr t e)

newtype FExp = HideF { revealF :: forall t e. Expr t e }

data Alt t e = ConstrAlt (Constructor t) [Src.ReadId] ( [e] -> Expr t e)
             | Default (Expr t e)

data DataBind t = DataBind Src.ReadId [Src.ReadId] ([t] -> [Constructor t])
data Constructor t = Constructor {constrName :: Src.ReadId, constrParams :: [Type t]}
-- newtype Typ = HideTyp { revealTyp :: forall t. Type t } -- type of closed types

-- newtype Exp = HideExp { revealExp :: forall t e. Expr t e }

type TypeContext t    = Set.Set t
type ValueContext t e = Map.Map e (Type t)

type Index = Int

alphaEq :: Int -> Type Index -> Type Index -> Bool
alphaEq _ (TVar _ a)   (TVar _ b)   = a == b
alphaEq _ (JClass c)   (JClass d)   = c == d
alphaEq i (Fun s1 s2)  (Fun t1 t2)  = alphaEq i s1 t1 && alphaEq i s2 t2
alphaEq i (Forall _ f) (Forall _ g) = alphaEq (succ i) (f i) (g i)
alphaEq i (Product ss) (Product ts) = length ss == length ts && uncurry (alphaEq i) `all` zip ss ts
alphaEq _  Unit     Unit            = True
alphaEq i (And s1 s2)  (And t1 t2)  = alphaEq i s1 t1 && alphaEq i s2 t2
alphaEq _ _            _            = False

mapTVar :: (Src.ReadId -> t -> Type t) -> Type t -> Type t
mapTVar g (TVar n a)     = g n a
mapTVar _ (JClass c)     = JClass c
mapTVar g (Fun t1 t2)    = Fun (mapTVar g t1) (mapTVar g t2)
mapTVar g (Forall n f)   = Forall n (mapTVar g . f)
mapTVar g (Product ts)   = Product (map (mapTVar g) ts)
mapTVar _  Unit          = Unit
mapTVar g (And t1 t2)    = And (mapTVar g t1) (mapTVar g t2)
mapTVar g (RecordType (l,t)) = RecordType (l, mapTVar g t)
mapTVar g (Datatype n ts ns)  = Datatype n (map (mapTVar g) ts) ns

mapVar :: (Src.ReadId -> e -> Expr t e) -> (Type t -> Type t) -> Expr t e -> Expr t e
mapVar g _ (Var n a)                 = g n a
mapVar _ _ (Lit n)                   = Lit n
mapVar g h (Lam n t f)               = Lam n (h t) (mapVar g h . f)
mapVar g h (BLam n f)                = BLam n (mapVar g h . f)
mapVar g h (Fix n1 n2 f t1 t)        = Fix n1 n2 (\x x1 -> mapVar g h (f x x1)) (h t1) (h t)
mapVar g h (Let n b e)               = Let n (mapVar g h b) (mapVar g h . e)
mapVar g h (LetRec ns ts bs e)       = LetRec ns (map h ts) (map (mapVar g h) . bs) (mapVar g h . e)
mapVar g h (Data rec databinds e)    = Data rec (map mapDatabind databinds) (mapVar g h e)
    where mapDatabind (DataBind name params ctrs) = DataBind name params (map mapCtr . ctrs)
          mapCtr (Constructor n ts) = Constructor n (map h ts)
mapVar g h (ConstrOut (Constructor n ts) es) = ConstrOut c' (map (mapVar g h) es)
    where c' = Constructor n (map h ts)
mapVar g h (Case e alts)             = Case (mapVar g h e) (map mapAlt alts)
    where mapAlt (ConstrAlt (Constructor n ts) ns e1) = ConstrAlt (Constructor n (map h ts)) ns ((mapVar g h) . e1)
          mapAlt (Default e1) = Default (mapVar g h e1)
mapVar g h (App f e)                 = App (mapVar g h f) (mapVar g h e)
mapVar g h (TApp f t)                = TApp (mapVar g h f) (h t)
mapVar g h (If p b1 b2)              = If (mapVar g h p) (mapVar g h b1) (mapVar g h b2)
mapVar g h (PrimOp e1 op e2)         = PrimOp (mapVar g h e1) op (mapVar g h e2)
mapVar g h (Tuple es)                = Tuple (map (mapVar g h) es)
mapVar g h (Proj i e)                = Proj i (mapVar g h e)
mapVar g h (JNew c args)             = JNew c (map (mapVar g h) args)
mapVar g h (JMethod callee m args c) = JMethod (fmap (mapVar g h) callee) m (map (mapVar g h) args) c
mapVar g h (JField  callee f c)      = JField (fmap (mapVar g h) callee) f (h c)
mapVar g h (Error ty str)            = Error (h ty) (mapVar g h str)
mapVar g h (Seq es)                  = Seq (map (mapVar g h) es)
mapVar g h (Merge e1 e2)             = Merge (mapVar g h e1) (mapVar g h e2)
mapVar g h (RecordCon (l, e))        = RecordCon (l, mapVar g h e)
mapVar g h (RecordProj e l)          = RecordProj (mapVar g h e) l
mapVar g h (RecordUpdate e (l1,e1))  = RecordUpdate (mapVar g h e) (l1, mapVar g h e1)
mapVar g h (Module pname defs) = Module pname (mapVarDefs defs)
  where
    -- necessary?
    mapVarDefs Null = Null
    mapVarDefs (Def n t expr def) = Def n t (mapVar g h expr) (mapVarDefs . def)
    mapVarDefs (DefRec names types exprs def) =
      DefRec names (map (second h) types) (map (mapVar g h) . exprs) (mapVarDefs . def)



fsubstTT :: Eq a => a -> Type a -> Type a -> Type a
fsubstTT x r = mapTVar (\n a -> if a == x then r else TVar n a)

fsubstTE :: Eq t => t -> Type t -> Expr t e -> Expr t e
fsubstTE x r = mapVar Var (fsubstTT x r)

fsubstEE :: Eq a => a -> Expr t a -> Expr t a -> Expr t a
fsubstEE x r = mapVar (\n a -> if a == x then r else Var n a) id


joinType :: Type (Type t) -> Type t
joinType (TVar n a)       = a
joinType (JClass c)       = JClass c
joinType (Fun t1 t2)      = Fun (joinType t1) (joinType t2)
joinType (Forall n g)     = Forall n (joinType . g . TVar "_") -- Right?
joinType (Product ts)     = Product (map joinType ts)
joinType  Unit            = Unit
joinType (And t1 t2)      = And (joinType t1) (joinType t2)
joinType (RecordType (l,t))   = RecordType (l, joinType t)
joinType (Datatype n ts ns)  = Datatype n (map joinType ts) ns

-- instance Show (Type Index) where
--   show = show . pretty

-- instance Pretty (Type Index) where
--   pretty = prettyType

prettyType :: Type Index -> Doc
prettyType = prettyType' basePrec 0

prettyType' :: Prec -> Index -> Type Index -> Doc

prettyType' _ _ (TVar n a)   = text n

prettyType' p i (Datatype n ts _) = hsep $ text n : map (prettyType' p i) ts

prettyType' p i (Fun t1 t2)  =
  parensIf p 2
    (prettyType' (2,PrecPlus) i t1 <+> arrow <+> prettyType' (2,PrecMinus) i t2)

prettyType' p i (Forall n f)   =
  parensIf p 1
    (forall <+> text n <> dot <+>
     prettyType' (1,PrecMinus) (succ i) (f i))

prettyType' _ i (Product ts) = parens $ hcat (intersperse comma (map (prettyType' basePrec i) ts))

prettyType' _ _  Unit = text "Unit"

prettyType' _ _ (JClass "java.lang.Integer")   = text "Int"
prettyType' _ _ (JClass "java.lang.String")    = text "String"
prettyType' _ _ (JClass "java.lang.Boolean")   = text "Bool"
prettyType' _ _ (JClass "java.lang.Character") = text "Char"
prettyType' _ _ (JClass c)                     = text c

prettyType' p i (And t1 t2) =
  parensIf p 2
    (prettyType' (2,PrecMinus) i t1 <+>
     ampersand  <+>
     prettyType' (2,PrecPlus) i t2)

prettyType' _ i (RecordType (l,t)) = lbrace <+> text l <+> colon <+> prettyType' basePrec i t <+> rbrace

-- instance Show (Expr Index Index) where
--   show = show . pretty

-- instance Pretty (Expr Index Index) where
--   pretty = prettyExpr

prettyDef :: Prec -> (Index, Index) -> Definition Index Index -> Doc
prettyDef _ (i, j) (Def fname typ e def) =
  text fname <+> colon <+> pretty (fst typ) <+> equals <+> prettyExpr' basePrec (i, j + 1) e <> semi <$>
  prettyDef basePrec (i, j+1) (def j) -- crappy pretty printer

prettyDef p (i, j) (DefRec names sigs binds def) = vcat (intersperse (text "and") pretty_binds) <> semi <$> pretty_body
  where
    n = length sigs
    ids = [i .. (i + n) - 1]
    pretty_ids = map text names
    pretty_sigs = map (pretty . fst) sigs
    pretty_defs = map (prettyExpr' p (i, j + n)) (binds ids)
    pretty_binds = zipWith3
                     (\pretty_id pretty_sig pretty_def ->
                        pretty_id <+> colon <+> pretty_sig <$> indent 2 (equals <+> pretty_def))
                     pretty_ids
                     pretty_sigs
                     pretty_defs
    pretty_body = prettyDef p (i, j + n) (def ids)

prettyDef _ _ Null = text ""

prettyExpr :: Expr Index Index -> Doc
prettyExpr = prettyExpr' basePrec (0, 0)

prettyExpr' :: Prec -> (Index, Index) -> Expr Index Index -> Doc

prettyExpr' _ _ (Var n _) = text n

prettyExpr' p (i,j) (Lam n t f)
  = parensIf p 2 $ group $ hang 2 $
      lambda <+> parens (text n <+> colon <+> prettyType' basePrec i t) <> dot <$>
      prettyExpr' (2,PrecMinus) (i, j + 1) (f j)

prettyExpr' p (i,j) (App e1 e2)
  = parensIf p 4 $
      group $ hang 2 $ prettyExpr' (4,PrecMinus) (i,j) e1 <$> prettyExpr' (4,PrecPlus) (i,j) e2

prettyExpr' p (i,j) (BLam n f) =
  parensIf p 2
    (biglambda <+> text n <> dot <+>
     prettyExpr' (2,PrecMinus) (succ i, j) (f i))

prettyExpr' p (i,j) (TApp e t) =
  parensIf p 4
    (group $ hang 2 $ prettyExpr' (4,PrecMinus) (i,j) e <$> prettyType' (4,PrecPlus) i t)

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

prettyExpr' p i (Module pname defs) =
  maybe empty ((text "package" <+>) . pretty) pname <$> text "module" <> semi <$> prettyDef p i defs

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

prettyExpr' p (i,j) (Seq es) = semiBraces (map (prettyExpr' p (i,j)) es)

prettyExpr' p (i,j) (Data recflag databinds e) =
  text "data" <+> (pretty recflag) <+> (align .vsep) (map prettyDatabind databinds) <$> prettyExpr' p (i,j) e
    where prettyCtr i' (Constructor ctrName ctrParams) = (text ctrName) <+> (hsep. map (prettyType' p i') $ ctrParams)
          prettyDatabind (DataBind n tvars cons) = hsep (map text $ n:tvars) <+> align
                 (equals <+> intersperseBar (map (prettyCtr (length tvars + i)) $ cons [i..length tvars +i-1]) <$$> semi)

prettyExpr' p i (Error _ str) = text "error:" <+> prettyExpr' p i str

prettyExpr' p (i,j) (Fix n1 n2 f t1 t)
  = parens $ group $ hang 2 $
      text "fix" <+> text n1 <+>
      parens (text n2 <+> colon <+> prettyType' p i t1) <+>
      colon <+> prettyType' p i t <> dot <$>
      prettyExpr' p (i, j + 2) (f j (j + 1))

prettyExpr' p (i,j) (Let n b e) =
  parensIf p 2 (text "let" <+> text n <+> equals <+> prettyExpr' basePrec (i, j + 1) b <$> text "in" <$>
  prettyExpr' basePrec (i, j + 1) (e j))

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

prettyExpr' p (i,j) (Merge e1 e2) =
  parens $ prettyExpr' p (i,j) e1 <+> dcomma <+> prettyExpr' p (i,j) e2

prettyExpr' _ (i,j) (RecordCon (l, e))       = lbrace <+> text l <+> equals <+> prettyExpr' basePrec (i,j) e <+> rbrace
prettyExpr' p (i,j) (RecordProj e l)         = prettyExpr' p (i,j) e <> dot <> text l
prettyExpr' p (i,j) (RecordUpdate e (l, e1)) = prettyExpr' p (i,j) e <+> text "with" <+> prettyExpr' p (i,j) (RecordCon (l, e1))

prettyExpr' p (i,j) (ConstrOut c es)            = parens $ hsep $ text (constrName c) : map (prettyExpr' p (i,j)) es

prettyExpr' p (i,j) (Case e alts) =
    hang 2 $ text "case" <+> prettyExpr' p (i,j) e <+> text "of" <$> align (intersperseBar (map pretty_alt alts))
    where pretty_alt (ConstrAlt c ns e1) =
            let n= length ns
                ids = [j..j+n-1]
            in
                hsep (text (constrName c) : map text ns ) <+> arrow <+> (align $ prettyExpr' p (i, j+n) (e1 ids))
          pretty_alt (Default e1) =
               (text "_" <+> arrow <+> (align $ prettyExpr' p (i, j) e1 ))

