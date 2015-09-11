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

-- import           Data.Char (toLower)
-- import           Data.List (zip4, elemIndex)
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
  where cu = J.CompilationUnit package [] compDef


initClass :: String -> String -> J.Exp -> J.BlockStmt
initClass className tempName expr =
  let ty = classTy className
  in localFinalVar
       ty
       (varDecl tempName
                (if ty == objClassTy
                    then expr
                    else cast ty expr))

type Var = Int -- Either normal variable or class name

type TransJavaExp = Either J.Name J.Literal -- either variable or special case: Lit

type TransType = ([J.BlockStmt], TransJavaExp, Type TransBind)

data TyBind = TB {unTB :: (Type (Var, TyBind))}

type TransBind = (Var, TyBind)

data Translate m =
  T {translateM :: Expr TransBind -> m TransType
    ,translateScopeM :: EScope TransBind -> m ([J.BlockStmt],TransJavaExp,EScope TransBind)
    ,translateApply :: Bool -> m TransType -> m TransType -> m TransType
    ,translateIf :: m TransType -> m TransType -> m TransType -> m TransType
    ,translateLet :: TransType -> (TransBind -> Expr TransBind) -> m TransType
    ,translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> EScope TransBind -> m ([J.BlockStmt],TransJavaExp,EScope TransBind) -> String -> m ([J.BlockStmt],EScope TransBind)
    ,genApply :: J.Exp -> EScope TransBind -> String -> J.Type -> J.Type -> m [J.BlockStmt]
    ,genRes :: EScope TransBind -> [J.BlockStmt] -> m [J.BlockStmt]
    ,applyRetType :: Type TransBind -> m (Maybe J.Type)
    ,genClone :: m Bool
    ,withApply :: m Bool
    ,getPrefix :: m String
    ,javaType :: Type TransBind -> m J.Type
    ,chooseCastBox :: Type TransBind -> m (String -> J.Exp -> J.BlockStmt,J.Type)
    ,stackMainBody :: Type Int -> m [J.BlockStmt]
    ,genClosureVar :: Bool -> Int -> TransJavaExp -> m J.Exp
    ,createWrap :: String -> Expr TransBind -> m (J.CompilationUnit,Type TransBind)}

getNewVarName :: MonadState Int m => Translate m -> m String
getNewVarName _ =
  do (n :: Int) <- get
     put (n + 1)
     return $ localvarstr ++ show n

assignVar :: Monad m => Translate m -> Type TransBind -> String -> J.Exp -> m J.BlockStmt
assignVar this t varId e =
  do aType <- javaType this t
     return $
       localFinalVar aType
                     (varDecl varId e)

getTupleClassName :: [a] -> String
getTupleClassName tuple =
  if lengthOfTuple > 50
     then panic "The length of tuple is too long (>50)!"
     else namespace ++ "tuples.Tuple" ++ show lengthOfTuple
  where lengthOfTuple = length tuple

genIfBody :: MonadState Int m
          => Translate m
          -> m TransType
          -> m TransType
          -> ([J.BlockStmt],TransJavaExp)
          -> Int
          -> m TransType
genIfBody this m2 m3 (s1,j1) n =
  do (s2,j2,t2) <- m2
     (s3,j3,_) <- m3
     let ifvarname = ifresultstr ++ show n
     aType <- javaType this t2
     let ifresdecl = localVar aType (varDeclNoInit ifvarname)
     let thenPart =
           J.StmtBlock $
           block (s2 ++
                  [bsAssign (name [ifvarname])
                            (unwrap j2)])
     let elsePart =
           J.StmtBlock $
           block (s3 ++
                  [bsAssign (name [ifvarname])
                            (unwrap j3)])
     let ifstmt =
           bStmt $
           J.IfThenElse (unwrap j1)
                        thenPart
                        elsePart
     return (s1 ++ [ifresdecl,ifstmt],var ifvarname,t2)

getS3 :: MonadState Int m
      => Translate m
      -> J.Exp
      -> J.Exp
      -> EScope TransBind
      -> J.Type
      -> m ([J.BlockStmt],TransJavaExp)
getS3 this j1 j2 retTyp ctempCastTyp =
  do (n :: Int) <- get
     put (n + 2)
     let f = localvarstr ++ show n
     let xf = localvarstr ++ show (n + 1)
     let fexp = left . var $ f
     let fd = localVar ctempCastTyp (varDecl f j1)
     let fs =
           assignField (fieldAccExp fexp closureInput)
                       j2
     (castBox,typ) <-
       chooseCastBox this
                     (scope2ctyp retTyp)
     apply <- genApply this fexp retTyp xf typ ctempCastTyp
     let fout = fieldAccess fexp closureOutput
     res <- genRes this retTyp [castBox xf fout]
     let r = fd : fs : apply ++ res
     return (r,var xf)


-- | Translation dispatcher
trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self =
  let this = up self
  in T {translateM = translateM' this
       ,translateScopeM = translateScopeM' this
       ,translateApply = translateApply' this
       ,translateIf = translateIf' this
       ,translateLet = undefined -- TODO: Li Huang
       ,translateScopeTyp = translateScopeTyp' this
       ,genApply = \f _ _ _ _ -> return [bStmt $ applyMethodCall f]
       ,genRes = const return
       ,applyRetType = applyRetType' this
       ,genClone = return False -- do not generate clone method
       ,withApply = return True
       ,getPrefix = return namespace
       ,javaType = javaType' this
       ,chooseCastBox = chooseCastBox' this
       ,stackMainBody = \_ -> return []
       ,genClosureVar = \_ _ j1 -> return (unwrap j1)
       ,createWrap = createWrap' this}

-- Field functions

translateM' this =
  \e ->
    case e of
      Var _ (i,t) -> return ([],var (localvarstr ++ show i),unTB t)
      Lit lit ->
        case lit of
          (S.Int i) -> return ([],Right $ J.Int i,JClass "java.lang.Integer")
          (S.UnitLit) -> return ([],Right J.Null,Unit)
          (S.String s) ->
            return ([],Right $ J.String s,JClass "java.lang.String")
          (S.Bool b) ->
            return ([],Right $ J.Boolean b,JClass "java.lang.Boolean")
          (S.Char c) ->
            return ([],Right $ J.Char c,JClass "java.lang.Character")
      PrimOp e1 op e2 ->
        do (s1,j1,_) <- translateM this e1
           (s2,j2,_) <- translateM this e2
           let j1' = unwrap j1
           let j2' = unwrap j2
           let (jexpr,typ) =
                 case op of
                   (S.Arith realOp) ->
                     (J.BinOp j1' realOp j2',JClass "java.lang.Integer")
                   (S.Compare realOp) ->
                     (J.BinOp j1' realOp j2',JClass "java.lang.Boolean")
                   (S.Logic realOp) ->
                     (J.BinOp j1' realOp j2',JClass "java.lang.Boolean")
           newVarName <- getNewVarName this
           assignExpr <- assignVar this typ newVarName jexpr
           return (s1 ++ s2 ++ [assignExpr],var newVarName,typ)
      If e1 e2 e3 ->
        translateIf this
                    (translateM this e1)
                    (translateM this e2)
                    (translateM this e3)
      Tuple tuple ->
        case tuple of
          [t] ->
            do (s1,j1,t1) <- translateM this t
               return (s1,j1,TupleType [t1])
          _ ->
            do tuple' <- mapM (translateM this) tuple
               let (statements,exprs,types) = unzip3 tuple' & _1 %~ concat
               newVarName <- getNewVarName this
               let c = getTupleClassName tuple
               let rhs =
                     instCreat (classTyp c)
                               (map unwrap exprs)
               assignExpr <- assignVar this (JClass c) newVarName rhs
               return (statements ++ [assignExpr]
                      ,var newVarName
                      ,TupleType types)
      Proj index expr ->
        do ret@(statement,javaExpr,exprType) <- translateM this expr
           case exprType of
             TupleType [_] -> return ret
             TupleType types ->
               do newVarName <- getNewVarName this
                  let typ = types !! (index - 1)
                  aType <- javaType this typ
                  let rhs =
                        cast aType
                             (fieldAccess (unwrap javaExpr)
                                          ("_" ++ show index))
                  assignExpr <- assignVar this typ newVarName rhs
                  return (statement ++ [assignExpr],var newVarName,typ)
             _ -> panic "BaseTransCFJava.trans: expected tuple type"
      App e1 e2 ->
        translateApply this
                       False
                       (translateM this e1)
                       (translateM this e2)
      Let _ expr body ->
        do (s1,j1,t1) <- translateM this expr
           translateLet this
                        (s1,j1,t1)
                        body
      Lam _ se ->
        do (s,je,t) <- translateScopeM this se
           return (s,je,Pi "_" t)
      Mu _ se -> undefined -- TODO: Mu thing
      SeqExprs es ->
        do es' <- mapM (translateM this) es
           let (_,lastExp,lastType) = last es'
           let statements = concatMap (\(x,_,_) -> x) es'
           return (statements,lastExp,lastType)
      -- Pi, TupleType, Unit, Star, Jnew, JMethod, JField, JClass, Error
      -- TODO: Li Huang: Finish all Java related
      _ -> panic "BaseTransCFJavaNew.trans: don't know how to do"

translateScopeM' this =
  \e ->
    case e of
      Body t ->
        do (s,je,t1) <- translateM this t
           return (s,je,Body t1)
      -- TODO: when t is star, should not generate code
      Type t g ->
        do n <- get
           let (x1,x2) = (n + 1,n + 2)
           put (x2 + 1)
           let nextInClosure = g (x2,TB t)
           typT1 <- javaType this t
           let flag = typT1 == objClassTy
           let accessField =
                 fieldAccess (left $ var (localvarstr ++ show x1))
                             closureInput
           let xf =
                 localFinalVar
                   typT1
                   (varDecl (localvarstr ++ show x2)
                            (if flag
                                then accessField
                                else cast typT1 accessField))
           closureClass <-
             liftM2 (++)
                    (getPrefix this)
                    (return "Closure")
           (cvar,t1) <-
             translateScopeTyp this
                               x1
                               n
                               [xf]
                               nextInClosure
                               (translateScopeM this nextInClosure)
                               closureClass
           let fstmt =
                 [localVar closureType
                           (varDecl (localvarstr ++ show n)
                                    (funInstCreate n))]
           return (cvar ++ fstmt,var (localvarstr ++ show n),Type t (const t1))

translateApply' this =
  \flag m1 m2 ->
    do (s1,j1',Pi _ (Type _ g)) <- m1
       (s2,j2,_) <- m2
       let retTyp = g undefined -- TODO: better choice?
       j1 <-
         genClosureVar this
                       flag
                       (getArity retTyp)
                       j1'
       (s3,nje3) <- getS3 this j1 (unwrap j2) retTyp closureType
       return (s2 ++ s1 ++ s3,nje3,scope2ctyp retTyp)

translateIf' this =
  \m1 m2 m3 ->
    do n <- get
       put (n + 1)
       (s1,j1,_) <- m1
       genIfBody this m2 m3 (s1,j1) n

translateScopeTyp' this =
  \x1 f initVars _ otherStmts closureClass ->
    do b <- genClone this
       (ostmts,oexpr,t1) <- otherStmts
       let fc = f
       return ([localClassDecl
                  (closureTransName ++ show fc)
                  closureClass
                  (closureBodyGen
                     [memberDecl $
                      fieldDecl (classTy closureClass)
                                (varDecl (localvarstr ++ show x1) J.This)]
                     (initVars ++
                      ostmts ++
                      [bsAssign (name [closureOutput])
                                (unwrap oexpr)])
                     fc
                     b
                     (classTy closureClass))]
              ,t1)

javaType' this =
  \typ ->
    case typ of
      (JClass c) -> return $ classTy c
      -- (Forall (Kind f)) -> case f 0 of -- TODO: could be a bug
      --                       Body typ' -> javaType this typ'
      --                       _ -> do closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
      -- return (classTy closureClass)
      -- TODO: need to check for big lambda
      (Pi _ s) ->
        do closureClass <-
             liftM2 (++)
                    (getPrefix this)
                    (return "Closure")
           return (classTy closureClass)
      (TupleType tuple) ->
        case tuple of
          [t] -> javaType this t
          _ -> return $ classTy $ getTupleClassName tuple
      _ -> return objClassTy

chooseCastBox' this =
  \typ ->
    case typ of
      (JClass c) -> return (initClass c,classTy c)
      (Pi _ _) ->
        do closureClass <-
             liftM2 (++)
                    (getPrefix this)
                    (return "Closure")
           return (initClass closureClass,classTy closureClass)
      (TupleType tuple) ->
        case tuple of
          [t] -> chooseCastBox this t
          _ ->
            do let tupleClassName = getTupleClassName tuple
               return (initClass tupleClassName,classTy tupleClassName)
      _ -> return (initClass "Object",objClassTy)

createWrap' this =
  \nam expr ->
    do (bs,e,t) <- translateM this expr
       returnType <- applyRetType this t
       let javaCode =
             let returnStmt = [bStmt $ J.Return $ Just (unwrap e)]
             in wrapperClass False
                             nam
                             (bs ++ returnStmt)
                             returnType
                             mainBody
       return ((createCUB Nothing [javaCode]),t)

applyRetType' _ =
  \t ->
    (case t of
       JClass "java.lang.Integer" -> return $ Just $ J.PrimType J.IntT
       JClass "java.lang.Boolean" -> return $ Just $ J.PrimType J.BooleanT
       _ -> return $ Just objClassTy)
