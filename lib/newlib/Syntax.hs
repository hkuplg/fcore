{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables #-}

module Syntax where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.List (zip4)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Unbound.Generics.LocallyNameless

import qualified Core as C
import           JavaUtils
import qualified Language.Java.Syntax as J
import           Panic
import qualified Src as S


type TmName = Name Expr

data Tele = Empty
          | Cons (Rebind (TmName, Embed Type) Tele)
  deriving (Show, Generic, Typeable)

type Scope = Bind Tele Expr

type JReceiver = Either ClassName Expr

type Type = Expr

-- | Syntax of the core
data Expr = Var TmName
          | App Expr Expr
          | Lam Scope
          | Pi Scope
          | Mu (Bind (TmName, Embed Type) Expr)
          | F Expr Expr
          | U Expr
          | Star

          | Let (Bind (TmName, Embed Type) Expr)
          | LetRec (Bind (Rec [(TmName, Embed Type, Embed Expr)]) Expr)
          | If Expr Expr Expr
          | Lit S.Lit
          | PrimOp S.Operator Expr Expr
          | Unit

          | Tuple [Expr]
          | Proj Int Expr

          | JClass ClassName

          | JNew ClassName [Expr]
          | JMethod JReceiver MethodName [Expr] ClassName
          | JField JReceiver FieldName Expr

          -- TODO: generalize to dependent pair
          | Product [Expr]

          | Seq [Expr]

          -- Module
          | Module (Maybe S.PackageName) Definition

  deriving (Show, Generic, Typeable)


data Definition = Def (Bind (TmName, Embed S.Type, Embed Expr) Definition)
                | DefRec (Bind (Rec [(TmName, Embed S.Type, Embed Type, Embed Expr)]) Definition)
                | DefNull
                deriving (Show, Generic, Typeable)

instance Alpha Expr
instance Alpha S.Lit
instance Alpha S.Operator
instance Alpha J.Op
instance Alpha S.Type
instance Alpha Tele
instance Alpha Definition

instance Subst Expr S.Operator
instance Subst Expr S.Lit
instance Subst Expr J.Op
instance Subst Expr Tele
instance Subst Expr Definition
instance Subst Expr S.Type

instance Subst Expr Expr where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing


-- smart constructors

evar :: String -> Expr
evar = Var . string2Name

elam :: [(TmName, Expr)] -> Expr -> Expr
elam t b = Lam (bind (mkTele t) b)

emu :: (String, Expr) -> Expr -> Expr
emu (n, t) b = Mu (bind (string2Name n, embed t) b)

edef :: (String, S.Type) -> Expr -> Definition -> Definition
edef (n, t) e b = Def (bind (string2Name n, embed t, embed e) b)

edefrec :: [(TmName, S.Type, Type, Expr)] -> Definition -> Definition
edefrec xs e = DefRec (bind binds e)
  where
    binds = rec (map (\(n, t1, t2, b) -> (n, embed t1, embed t2, embed b)) xs)

epi :: [(TmName, Expr)] -> Expr -> Expr
epi t b = Pi (bind (mkTele t) b)

earr :: Expr -> Expr -> Expr
earr t1 = epi [(string2Name "_", t1)]

estar :: Expr
estar = Star

eapp :: Expr -> Expr -> Expr
eapp = App

elet :: String -> Expr -> Expr -> Expr
elet n e1 e2 = Let (bind (s2n n, embed e1) e2)

eletrec :: [(TmName, Type, Expr)] -> Expr -> Expr
eletrec xs e = LetRec (bind binds e)
  where binds = rec (map (\(n, t, b) -> (n, embed t, embed b)) xs)

mkTele :: [(TmName, Expr)] -> Tele
mkTele []          = Empty
mkTele ((x,e) : t) = Cons (rebind (x, Embed e) (mkTele t))


-- Old Core to New Core Translation

core2New :: Fresh m => C.Expr TmName TmName -> m Expr
core2New = transExpr
  where
    -- TODO: missing datatype, letrec
    transExpr :: Fresh m => C.Expr TmName TmName -> m Expr
    transExpr (C.Var _ e) = return $ Var e
    transExpr (C.Lit l) = return $ Lit l

    transExpr e@(C.Lam n t f) = do
      (tele, body) <- getLambda e
      return $ elam tele body

    transExpr (C.Fix n1 n2 f t1 t) = do
      fun <- transType (C.Fun t1 t)
      t1' <- transType t1
      n1' <- fresh (string2Name n1)
      n2' <- fresh (string2Name n2)
      f' <- transExpr (f n1' n2')
      return $ Mu (bind (n1', Embed fun) (elam [(n2', t1')] f'))

    transExpr (C.Let n b e) = do
      letName <- fresh (string2Name n)
      b' <- transExpr b
      e' <- transExpr (e letName)
      return (Let (bind (letName, Embed b') e'))

    transExpr (C.LetRec ns ts bs body) = do
      ns' <- mapM (fresh . s2n) ns
      ts' <- mapM transType ts
      bs' <- mapM transExpr (bs ns')
      body' <- transExpr (body ns')
      return $ eletrec (zip3 ns' ts' bs') body'

    transExpr e@(C.BLam n f) = do
      (tele, body) <- getLambda e
      return $ elam tele body
    transExpr (C.App f e) = eapp <$> (transExpr f) <*> (transExpr e)
    transExpr (C.TApp f e) = eapp <$> (transExpr f) <*> (transType e)
    transExpr (C.If e1 e2 e3) = If <$> (transExpr e1) <*> (transExpr e2) <*> (transExpr e3)
    transExpr (C.PrimOp e1 op e2) = PrimOp op <$> (transExpr e1) <*> (transExpr e2)
    transExpr (C.JNew name args) = JNew name <$> (mapM transExpr args)
    transExpr (C.JMethod rcv mname args cname) =
      do
        args' <- mapM transExpr args
        rcv' <- transRecv rcv
        return (JMethod rcv' mname args' cname)
    transExpr (C.JField rcv fname t) =
      do
        rcv' <- transRecv rcv
        t' <- transType t
        return (JField rcv' fname t')
    transExpr (C.Tuple es) = do
      es' <- mapM transExpr es
      return $ Tuple es'
    transExpr (C.Proj i e) = do
      e' <- transExpr e
      return $ Proj i e'
    transExpr (C.Seq es) = do
      es' <- mapM transExpr es
      return $ Seq es'

    transExpr (C.Module n defs) = Module n <$> (trans defs)
      where
        trans C.Null = return DefNull
        trans (C.Def fname ty e def) = do
          e' <- transExpr e
          fname' <- fresh (s2n fname)
          def' <- trans (def fname')
          return $ edef (fname, ty) e' def'
        trans (C.DefRec names tys es defs) = do
          names' <- mapM (fresh . s2n) names
          let t1' = map fst tys
          t2' <- mapM (transType . snd) tys
          es' <- mapM transExpr (es names')
          defs' <- trans (defs names')
          return $ edefrec (zip4 names' t1' t2' es') defs'

    transExpr _ = sorry "Syntax.transExpr: not defined"

    transRecv :: Fresh m => S.JReceiver (C.Expr TmName TmName) -> m JReceiver
    transRecv (S.Static c) = return (Left c)
    transRecv (S.NonStatic e) = do
      e' <- (transExpr e)
      return (Right e')

    transType :: Fresh m => C.Type TmName -> m Type
    transType (C.TVar _ x) = return $ Var x
    transType (C.JClass c) = return $ JClass c
    transType (C.Fun t1 t2) = do
      t1' <- transType t1
      t2' <- transType t2
      return $ earr t1' t2'
    transType (C.Forall n f) = do
      forName <- fresh (string2Name n)
      f' <- transType (f forName)
      return $ epi [(forName, Star)] f'
    transType C.Unit = return Unit
    transType (C.Product ts) = do
      ts' <- mapM transType ts
      return $ Product ts'
    transType _ = sorry "Syntax.transType: not defined"

    getLambda :: Fresh m => C.Expr TmName TmName -> m ([(TmName, Expr)], Expr)
    getLambda = collect []
      where
        collect bag (C.Lam n t f) = do
          n' <- fresh (string2Name n)
          t' <- transType t
          collect ((n', t') : bag) (f n')
        collect bag (C.BLam n f) = do
          n' <- fresh (string2Name n)
          collect ((n', Star) : bag) (f n')
        collect bag e = do
          e' <- transExpr e
          return (reverse bag, e')


{-
-- even and odd
evenOdd :: Expr
evenOdd =
  let even = s2n "even"
      odd = s2n "odd"
  in eletrec
       [ (even, (earr javaInt javaInt), elam [(x, javaInt)]
                                          (If
                                             (PrimOp (S.Compare J.Equal) (Var x) (Lit (S.Int 0)))
                                             (Lit (S.Int 0))
                                             (App (Var odd)
                                                (PrimOp (S.Arith J.Sub) (Var x) (Lit (S.Int 1))))))
       , (odd, (earr javaInt javaInt), elam [(x, javaInt)]
                                         (If
                                            (PrimOp (S.Compare J.Equal) (Var x) (Lit (S.Int 0)))
                                            (Lit (S.Int 1))
                                            (App (Var even)
                                               (PrimOp (S.Arith J.Sub) (Var x) (Lit (S.Int 1))))))
       ]
       (App (Var odd) (Lit (S.Int 3)))

x :: TmName
y :: TmName
z :: TmName
(x,y,z) = (string2Name "x", string2Name "y", string2Name "z")

javaInt :: Expr
javaInt = JClass "Integer"

javaBool :: Expr
javaBool = JClass "Bool"

-}
