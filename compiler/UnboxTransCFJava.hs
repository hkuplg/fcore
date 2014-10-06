{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module UnboxTransCFJava where

import Prelude hiding (init, last)

import qualified Data.Set as Set

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

transUnbox :: (MonadState Int m, selfType :< UnboxTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (UnboxTranslate m)
transUnbox this super = UT {toUT = super {
  translateM = \e -> case e of
       TApp e CFInt ->
         do  n <- get
             (s,je, Forall (Kind f)) <- translateM (up this) e
             return (s,je, scope2ctyp (substScope n CFInteger (f n)))

       App e1 e2 -> translateApply (up this) (translateM (up this) e1) (translateM (up this) e2)

       Lit lit -> case lit of
                    (Src.Integer i) -> return ([], J.Lit $ J.Int i, CFInt)
                    _ -> translateM super e

       PrimOp e1 op e2 ->
         do (s1, j1, _) <- translateM (up this) e1
            (s2, j2, _) <- translateM (up this) e2
            let (je, typ) = case op of
                              (Src.Arith realOp)   -> (J.BinOp j1 realOp j2, CFInt)
                              (Src.Compare realOp) -> (J.BinOp j1 realOp j2, JClass "java.lang.Boolean")
                              (Src.Logic realOp)   -> (J.BinOp j1 realOp j2, JClass "java.lang.Boolean")
            newVarName <- getNewVarName this
            return (s1 ++ s2 ++ [assignVar newVarName je typ], var newVarName, typ)

       otherwise   -> translateM super e,

  translateScopeM = \e m -> case e of
--PrimType IntT
{-
  getCvarAss = \t f j1 j2 ->
     return [ J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp j1))]),
              J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA j2) )],

\currentId nextId initVars nextInClosure m ->
translateScopeTyp this v n [js] nextInClosure (translateScopeM this nextInClosure Nothing)
-}
      Type t g ->
        do  n <- get
            let f       = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put (n' + 1)
            let nextInClosure = g (n',t)
            let aType = case t of CFInt -> J.PrimType J.IntT
                                  CFInteger -> javaType (JClass "java.lang.Integer")
                                  _ -> javaType t
            let js = (initStuff localvarstr n' (inputFieldAccess (localvarstr ++ show v)) aType)
            (statementsBeforeOA,javaExpression,t1) <- translateScopeM (up this) nextInClosure Nothing
            b <- genClone (up this)
            let cName = getClassType (scope2ctyp t1) t
            let currentId = v
            let nextId = n
            let initVars = [js]
            let cvar = [J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
                 (Just $ J.ClassRefType (J.ClassType [(J.Ident cName,[])])) [] (jexp [currentInitialDeclaration
                 (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (initVars ++ statementsBeforeOA ++ [outputAssignment javaExpression]))) nextId b)),
                 J.LocalVars [] (classTy cName) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (funInstCreate nextId)))])]
            return (cvar,J.ExpName (J.Name [f]), Type t (\_ -> t1) )

      otherwise   -> translateScopeM super e m,

  translateApply = \m1 m2 ->
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1, Forall (Type t1 g)) <- m1
           (s2,j2,t2) <- m2
           let t    = g ()
           let cName = getClassType t1 (scope2ctyp t)
           let (wrapS, jS) = wrap j2 t1 t2
           let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
           cvarass <- getCvarAss (up this) t f j1 jS
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           (s3, nje3) <- getS3 (up this) f t j3 cvarass
           return (s1 ++ s2 ++ wrapS ++ s3, nje3, scope2ctyp t)


}}
