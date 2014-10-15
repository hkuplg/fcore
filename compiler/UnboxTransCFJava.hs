{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module UnboxTransCFJava where

import Prelude hiding (init, last)

-- import qualified Data.Set as Set

import qualified Language.Java.Syntax as J
import ClosureF
import Inheritance
import BaseTransCFJava
import StringPrefixes
import MonadLib
import JavaUtils
import JavaEDSL
import qualified Src

data UnboxTranslate m = UT {toUT :: Translate m}

instance (:<) (UnboxTranslate m) (Translate m) where
   up              = up . toUT

instance (:<) (UnboxTranslate m) (UnboxTranslate m) where
   up              = id

getClassType :: Type t -> Type t -> ClassName
getClassType CFInt CFInt = closureClass ++ "IntInt"
getClassType CFInt _ = closureClass ++ "IntBox"
getClassType _ CFInt = closureClass ++ "BoxInt"
getClassType _ _ = closureClass ++ "BoxBox"

--TODO: generate wrappers when types T1~T2
wrap :: J.Exp -> Type t -> Type t -> ([J.BlockStmt], J.Exp)
wrap e t1 t2 = ([], e)

javaType2 CFInt             = J.PrimType J.IntT --classTy "java.lang.Integer"
javaType2 (Forall (Type t1 f))  = case (f ()) of (Body t2) -> classTy (getClassType t1 t2)
                                                 _ -> classTy (getClassType t1 CFInteger)
javaType2 x                 = javaType x

assignVar2 :: String -> J.Exp -> Type t -> J.BlockStmt
assignVar2 varId e t = J.LocalVars [] (javaType2 t) [varDecl varId e]

genIfBody2 :: Monad m
          => t
          -> m ([J.BlockStmt],J.Exp,Type t2)
          -> m ([J.BlockStmt],J.Exp,t1)
          -> J.Exp
          -> [J.BlockStmt]
          -> Int
          -> m ([J.BlockStmt],J.Exp,Type t2)
genIfBody2 _ m2 m3 j1 s1 n =
  do (s2,j2,t2) <- m2 {-translateM this e2-}
     (s3,j3,t3) <- m3 {-translateM this e3-}
     let ifvarname = (ifresultstr ++ show n)
     let ifresdecl = localVar (javaType t2) (varDeclNoInit ifvarname)
     let thenPart = (J.StmtBlock $ block (s2 ++ [assign (name [ifvarname]) j2]))
     let elsePart = (J.StmtBlock $ block (s3 ++ [assign (name [ifvarname]) j3]))
     let ifstmt  = bStmt $ J.IfThenElse j1 thenPart elsePart
     return (s1 ++ [ifresdecl,ifstmt],var ifvarname ,t2) -- need to check t2 == t3

chooseCastBox2 :: Type t -> (String -> Int -> J.Exp -> J.BlockStmt, J.Type)
chooseCastBox2 (CFInt) =
  (initClass "int",J.PrimType J.IntT)
chooseCastBox2 (CFInteger) =
  (initClass "java.lang.Integer",classTy "java.lang.Integer")
chooseCastBox2 (Forall (Type t1 f)) =
  case (f ()) of
    (Body t2) ->
      (initClass (getClassType t1 t2),classTy (getClassType t1 t2))
    _ ->
      (initClass (getClassType t1 CFInteger),classTy (getClassType t1 CFInteger))
chooseCastBox2 t = chooseCastBox t

getS3_2 :: MonadState Int m => Translate m -> J.Ident -> TScope Int -> J.Exp -> [J.BlockStmt] -> m ([J.BlockStmt], J.Exp)
getS3_2 this func t j3 cvarass =
  do (n :: Int) <- get
     put (n+1)
     let (cast,typ) = chooseCastBox2 (scope2ctyp t)
     apply <- genApply this func t (var (tempvarstr ++ show n)) typ
     rest <- genRes this t [cast tempvarstr n j3]
     let r = cvarass ++ apply ++ rest
     return (r, var (tempvarstr ++ show n))

transUnbox :: (MonadState Int m, selfType :< UnboxTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (UnboxTranslate m)
transUnbox this super =
  UT {toUT =
        super {translateM =
                 \e ->
                   case e of
                     TApp e CFInt ->
                       do n <- get
                          (s,je,Forall (Kind f)) <- translateM (up this) e
                          return (s,je,scope2ctyp (substScope n CFInteger (f n)))
                     App e1 e2 ->
                       translateApply (up this) (translateM (up this) e1) (translateM (up this) e2)
                     Lit lit ->
                       case lit of
                         (Src.Integer i) -> return ([] ,J.Lit $ J.Int i ,CFInt)
                         _ -> translateM super e
                     PrimOp e1 op e2 ->
                       do (s1,j1,_) <- translateM (up this) e1
                          (s2,j2,_) <- translateM (up this) e2
                          let (je,typ) =
                                case op of
                                  (Src.Arith realOp) -> (J.BinOp j1 realOp j2,CFInt)
                                  (Src.Compare realOp) -> (J.BinOp j1 realOp j2,JClass "java.lang.Boolean")
                                  (Src.Logic realOp) -> (J.BinOp j1 realOp j2,JClass "java.lang.Boolean")
                          newVarName <- getNewVarName this
                          return (s1 ++ s2 ++ [assignVar2 newVarName je typ]
                                 ,var newVarName
                                 ,typ)
                     _ -> translateM super e
              ,translateScopeM = \e m -> case e of
                   Type t g ->
                     do  n <- get
                         let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
                         put (n' + 1)
                         let nextInClosure = g (n',t)

                         let aType = javaType2 t
                         let accessField = fieldAccess (var (localvarstr ++ show v)) closureInput
                         -- let js = localFinalVar aType (varDecl (localvarstr ++ show n')
                         --                                       (if flag
                         --                                           then accessField
                         --                                           else cast aType accessField))
                         let js = localFinalVar aType (varDecl (localvarstr ++ show n') (cast aType accessField))

                         b <- genClone (up this)
                         let currentId = v
                         let oldId = n
                         let initVars = [js]
                         (ostmts,oexpr,t1) <- translateScopeM (up this) nextInClosure Nothing
                         let cName = getClassType t (scope2ctyp t1)
                         let cvar =
                               [localClassDecl ("Fun" ++ show oldId)
                                               cName
                                               (closureBodyGen [memberDecl $ fieldDecl (classTy cName)
                                                                                       (varDecl (localvarstr ++ show currentId) J.This)]
                                                               (initVars ++ ostmts ++ [assign (name ["out"]) oexpr])
                                                               oldId
                                                               b)
                               ,localVar (classTy cName) (varDecl (localvarstr ++ show oldId) (funInstCreate oldId))]
                         return (cvar,var (localvarstr ++ show n), Type t (\_ -> t1) )

                   _ -> translateScopeM super e m
              ,translateApply = \m1 m2 ->
                    do  (n :: Int) <- get
                        put (n+1)
                        (s1,j1, Forall (Type t1 g)) <- m1
                        (s2,j2,t2) <- m2
                        let t    = g ()
                        let cName = getClassType t1 (scope2ctyp t)
                        let (wrapS, jS) = wrap j2 t1 t2
                        let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
                        let cvarass = [J.LocalVars
                                   []
                                   (classTy cName)
                                   ([J.VarDecl (J.VarId f)
                                               (Just (J.InitExp j1))])
                                ,J.BlockStmt
                                   (J.ExpStmt (J.Assign (J.FieldLhs
                                                           (J.PrimaryFieldAccess (J.ExpName (J.Name [f]))
                                                                                 (J.Ident localvarstr)))
                                                        J.EqualA
                                                        jS))]
                        let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
                        (s3, nje3) <- getS3_2 (up this) f t j3 cvarass
                        return (s1 ++ s2 ++ wrapS ++ s3, nje3, scope2ctyp t)
              ,translateIf =
                 \m1 m2 m3 ->
                   do n <- get
                      put (n + 1)
                      (s1,j1,t1) <- m1 {- translateM this e1 -}
                      -- let j1' = J.BinOp j1 J.Equal (J.Lit (J.Int 0))
                      -- genIfBody this m2 m3 j1' s1 n,
                      genIfBody2 this m2 m3 j1 s1 n}}
