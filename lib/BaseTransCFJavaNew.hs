{-# LANGUAGE FlexibleContexts, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables #-}
{- |
Module      :  BaseTransCFJavaNew
Description :  Basic translation of dependently typed FCore to Java
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Tomas <tomtau@connect.hku.hk>
Stability   :  stable
Portability :  non-portable (MPTC)

This module implements the basic translation of FCore to Java. For
more information, please refer to the paper on wiki.
-}


module BaseTransCFJavaNew where

import           Data.Char (toLower)
import           Data.List (zip4, elemIndex)
import qualified Language.Java.Syntax as J
import           Lens.Micro


import           ClosureFNew
import           Inheritance
import           JavaEDSL
import           MonadLib hiding (Alt)
import           Panic
import qualified Src as S
import           StringPrefixes

instance (:<) (Translate m) (Translate m) where
   up = id

type InitVars = [J.BlockStmt]

createCUB :: Maybe J.PackageDecl -> [J.TypeDecl] -> J.CompilationUnit
createCUB package compDef = cu
  where
    cu = J.CompilationUnit package [] compDef


initClass :: String -> String -> J.Exp -> J.BlockStmt
initClass className tempName expr =
    let ty = classTy className
    in localFinalVar
           ty
           (varDecl
                tempName
                (if ty == objClassTy
                     then expr
                     else cast ty expr))

type Var = Int -- Either normal variable or class name

type TransJavaExp = Either J.Name J.Literal -- either variable or special case: Lit

type TransType = ([J.BlockStmt], TransJavaExp, Type Int)

data Translate m = T
    { translateM :: Expr (Var, Type Int) -> m TransType
    , translateScopeM :: EScope (Var, Type Int) -> Maybe (Int, Type Int) -> m ([J.BlockStmt], TransJavaExp, EScope Int)
    , translateApply :: Bool -> m TransType -> m TransType -> m TransType
    , translateIf :: m TransType -> m TransType -> m TransType -> m TransType
    , translateLet :: TransType -> ((Var, Type Int) -> Expr (Var, Type Int)) -> m TransType
    , translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> EScope (Var, Type Int) -> m ([J.BlockStmt], TransJavaExp, EScope Int) -> String -> m ([J.BlockStmt], EScope Int)
    , genApply :: J.Exp -> EScope Int -> String -> J.Type -> J.Type -> m [J.BlockStmt]
    , genRes :: EScope Int -> [J.BlockStmt] -> m [J.BlockStmt]
    , applyRetType :: Type Int -> m (Maybe J.Type)
    , genClone :: m Bool
    , withApply :: m Bool
    , getPrefix :: m String
    , javaType :: Type Int -> m J.Type
    , chooseCastBox :: Type Int -> m (String -> J.Exp -> J.BlockStmt, J.Type)
    , stackMainBody :: Type Int -> m [J.BlockStmt]
    , genClosureVar :: Bool -> Int -> TransJavaExp -> m J.Exp
    , createWrap :: String -> Expr (Var, Type Int) -> m (J.CompilationUnit, Type Int)
    }

getNewVarName :: MonadState Int m => Translate m -> m String
getNewVarName _ = do
    (n :: Int) <- get
    put (n + 1)
    return $ localvarstr ++ show n

assignVar :: Monad m => Translate m -> Type Int -> String -> J.Exp -> m J.BlockStmt
assignVar this t varId e = do
    aType <- javaType this t
    return $ localFinalVar aType (varDecl varId e)

getTupleClassName :: [a] -> String
getTupleClassName tuple =
    if lengthOfTuple > 50
        then panic "The length of tuple is too long (>50)!"
        else namespace ++ "tuples.Tuple" ++ show lengthOfTuple
  where
    lengthOfTuple = length tuple

trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self =
    let this = up self
    in T
       { translateM = \e ->
                           case e of
                               Var _ (i,t) ->
                                   return ([], var (localvarstr ++ show i), t)
                               Lit lit ->
                                   case lit of
                                       (S.Int i) ->
                                           return
                                               ( []
                                               , Right $ J.Int i
                                               , JClass "java.lang.Integer")
                                       (S.UnitLit) ->
                                           return ([], Right J.Null, Unit)
                                       (S.String s) ->
                                           return
                                               ( []
                                               , Right $ J.String s
                                               , JClass "java.lang.String")
                                       (S.Bool b) ->
                                           return
                                               ( []
                                               , Right $ J.Boolean b
                                               , JClass "java.lang.Boolean")
                                       (S.Char c) ->
                                           return
                                               ( []
                                               , Right $ J.Char c
                                               , JClass "java.lang.Character")
                               PrimOp e1 op e2 -> do
                                   (s1,j1,_) <- translateM this e1
                                   (s2,j2,_) <- translateM this e2
                                   let j1' = unwrap j1
                                   let j2' = unwrap j2
                                   let (jexpr,typ) =
                                           case op of
                                               (S.Arith realOp) ->
                                                   ( J.BinOp j1' realOp j2'
                                                   , JClass "java.lang.Integer")
                                               (S.Compare realOp) ->
                                                   ( J.BinOp j1' realOp j2'
                                                   , JClass "java.lang.Boolean")
                                               (S.Logic realOp) ->
                                                   ( J.BinOp j1' realOp j2'
                                                   , JClass "java.lang.Boolean")
                                   newVarName <- getNewVarName this
                                   assignExpr <-
                                       assignVar this typ newVarName jexpr
                                   return
                                       ( s1 ++ s2 ++ [assignExpr]
                                       , var newVarName
                                       , typ)
                               If e1 e2 e3 ->
                                   translateIf
                                       this
                                       (translateM this e1)
                                       (translateM this e2)
                                       (translateM this e3)
                               Tuple tuple ->
                                   case tuple of
                                       [t] -> do
                                           (s1,j1,t1) <- translateM this t
                                           return (s1, j1, TupleType [t1])
                                       _ -> do
                                           tuple' <-
                                               mapM (translateM this) tuple
                                           let (statements,exprs,types) =
                                                   (unzip3 tuple') & _1 %~
                                                   concat
                                           newVarName <- getNewVarName this
                                           let c = getTupleClassName tuple
                                           let rhs =
                                                   instCreat
                                                       (classTyp c)
                                                       (map unwrap exprs)
                                           assignExpr <-
                                               assignVar
                                                   this
                                                   (JClass c)
                                                   newVarName
                                                   rhs
                                           return
                                               ( statements ++ [assignExpr]
                                               , var newVarName
                                               , TupleType types)
                               Proj index expr -> do
                                   ret@(statement,javaExpr,exprType) <-
                                       translateM this expr
                                   case exprType of
                                       TupleType [_] -> return ret
                                       TupleType types -> do
                                           newVarName <- getNewVarName this
                                           let typ = types !! (index - 1)
                                           aType <- javaType this typ
                                           let rhs =
                                                   cast
                                                       aType
                                                       (fieldAccess
                                                            (unwrap javaExpr)
                                                            ("_" ++ show index))
                                           assignExpr <-
                                               assignVar
                                                   this
                                                   typ
                                                   newVarName
                                                   rhs
                                           return
                                               ( statement ++ [assignExpr]
                                               , var newVarName
                                               , typ)
                                       _ ->
                                           panic
                                               "BaseTransCFJava.trans: expected tuple type"
                               App e1 e2 ->
                                   translateApply
                                       this
                                       False
                                       (translateM this e1)
                                       (translateM this e2)
                               Let _ expr body -> do
                                   (s1,j1,t1) <- translateM this expr
                                   translateLet this (s1, j1, t1) body
                               Lam _ se -> do
                                   (s,je,t) <- translateScopeM this se Nothing
                                   return (s, je, Pi "_" t)
                               Mu _ se -> undefined -- TODO: Mu thing
                               SeqExprs es -> do
                                   es' <- mapM (translateM this) es
                                   let (_,lastExp,lastType) = last es'
                                   let statements =
                                           concatMap
                                               (\(x,_,_) ->
                                                     x)
                                               es'
                                   return (statements, lastExp, lastType)
                               -- Pi, TupleType, Unit, Star, Jnew, JMethod, JField, JClass, Error
                               _ ->
                                   panic
                                       "BaseTransCFJavaNew.trans: don't know how to do"
       , translateScopeM = undefined
       , translateApply = undefined
       , translateIf = undefined
       , translateLet = undefined
       , translateScopeTyp = undefined
       , genApply = undefined
       , genRes = undefined
       , applyRetType = undefined
       , genClone = undefined
       , withApply = undefined
       , getPrefix = undefined
       , javaType = undefined
       , chooseCastBox = undefined
       , stackMainBody = undefined
       , genClosureVar = undefined
       , createWrap = undefined
       }
