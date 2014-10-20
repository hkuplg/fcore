{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module BenchGenStack where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import Inheritance
--import qualified Data.Map as Map
--import qualified Data.Set as Set
import ClosureF
import BaseTransCFJava
import StackTransCFJava
import BenchGenCF2J
-- import StringPrefixes
import MonadLib
import JavaEDSL


whileApplyLoopB :: (Monad m) => Translate m -> String -> String -> J.Type -> J.Type -> m [J.BlockStmt]
whileApplyLoopB this ctemp tempOut outType ctempCastTyp = do
    nextName <- nextClass this
    return [bStmt (J.Do (J.StmtBlock (block [assign (name [ctemp]) (J.ExpName $ name [nextName, "next"])
                                            ,assign (name [nextName, "next"]) (J.Lit J.Null)
                                            ,bStmt (methodCall [ctemp, "apply"] [])]))
                   (J.BinOp (J.ExpName $ name [nextName, "next"])
                    J.NotEq
                    (J.Lit J.Null))),
            assign (name [tempOut]) (cast outType (J.FieldAccess (fieldAccExp (cast ctempCastTyp (var ctemp)) "out")))]

--c = (Closure) result;
--c.x = x;
--Next.next = c;
passClousre from to param = [
  (J.BlockStmt $ J.ExpStmt $ J.Assign (J.NameLhs $ J.Name [(J.Ident to)]) J.EqualA (J.Cast closureType (J.ExpName $ J.Name [(J.Ident from)]))),
  paraAssign to param,
  (J.BlockStmt $ J.ExpStmt $ J.Assign (J.FieldLhs $ (fieldAcc "hk.hku.cs.f2j.Next" "next")) (J.EqualA) (J.ExpName $ J.Name [(J.Ident to)]))]

retResStack :: String -> String -> J.BlockStmt
retResStack returnType id = (J.BlockStmt (J.Return $ Just (J.Cast (classTy returnType) (J.ExpName $ J.Name [(J.Ident id)]))))


testfuncBody :: Monad m => Translate m -> [String] -> m [J.BlockStmt]
testfuncBody this paraType = do closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
                                let closureType' = classTy closureClass
                                let lastClosure = localVar closureType' (varDeclNoInit "c")
                                let resDef = localVar objClassTy (varDecl "result" (J.Lit J.Null))
                                loop <- whileApplyLoopB this "c" "result" objClassTy closureType'
                                case paraType of
                                 [] -> return []
                                 x : [] -> return ([applyCall,lastClosure,resDef]
                                                   ++ loop
                                                   ++ passClousre "result" "c" "x0"
                                                   ++ loop
                                                   ++ [retResStack "Integer" "result"])
                                 x : y : [] -> return ([applyCall, lastClosure, resDef]
                                                       ++ loop
                                                       ++ passClousre "result" "c" "x0"
                                                       ++ loop
                                                       ++ passClousre "result" "c" "x1"
                                                       ++ loop
                                                       ++ [retResStack "Integer" "result"])
                                 _ -> return []


createCUB :: t -> [J.TypeDecl] -> J.CompilationUnit
createCUB _ compDef = cu where
   cu = J.CompilationUnit (benchmarkPackage "benchmark") [] (compDef)
-- [closureClass this] ++ !!!

--date type for stack BenchGen
data BenchGenTranslateStack m = TBS {
  toTBS :: Translate m -- supertype is a subtype of Translate (later on at least)
}

instance (:<) (BenchGenTranslateStack m) (Translate m) where
   up = up . toTBS

instance (:<) (BenchGenTranslateStack m) (BenchGenTranslateStack m) where
  up = id



transBenchStack :: (MonadState Int m, selfType :< BenchGenTranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (BenchGenTranslateStack m)
transBenchStack this super = TBS {
  toTBS = super {

  createWrap = \name exp ->
        do (bs,e,t) <- translateM super exp
           let returnType = case t of JClass "java.lang.Integer" -> Just $ J.PrimType $ J.IntT
                                      _ -> Just objClassTy
           let paraType = getParaType t
           --let classDecl = BenchGenCF2J.getClassDecl name bs ([J.BlockStmt (J.Return $ Just maybeCastedReturnExp)]) paraType BenchGenStack.testfuncBody returnType mainbody
           empyClosure' <- empyClosure (up this) e ""
           stackbody' <- stackMainBody (up this) t
           testBody <- BenchGenStack.testfuncBody (up this) paraType
           isTest <- genTest (up this)
           let stackDecl = wrapperClass name (bs ++ (if (containsNext bs) then [] else [empyClosure'])) Nothing (Just $ J.Block $ stackbody') (genParams paraType) (Just (J.Block testBody)) isTest
           return (BenchGenStack.createCUB super [stackDecl], t)
   }
}

--date type for stack + apply opt BenchGen
data BenchGenTranslateStackOpt m = TBSA {
  toTBSA :: Translate m -- supertype is a subtype of Translate (later on at least)
}

instance (:<) (BenchGenTranslateStackOpt m) (Translate m) where
   up = up . toTBSA

instance (:<) (BenchGenTranslateStackOpt m) (BenchGenTranslateStackOpt m) where
  up = id

wrapperClassB :: String -> [J.BlockStmt] -> Maybe J.Type -> Maybe J.Block -> [J.FormalParam] -> Maybe J.Block -> Bool -> J.TypeDecl
wrapperClassB className stmts returnType mainbodyDef testArgType testBodyDef genTest =
  J.ClassTypeDecl
    (classDecl [J.Public]
               className
               (classBody (applyMethod : mainMethod : if genTest then [testMethod] else [])))
  where body = Just (block stmts)
        applyMethod = memberDecl $ methodDecl [J.Static] Nothing "apply" [] body
        testMethod = memberDecl $ methodDecl [J.Public,J.Static] returnType "test" testArgType testBodyDef
        mainMethod  = memberDecl $ methodDecl [J.Public,J.Static] Nothing "main" mainArgType mainbodyDef

transBenchStackOpt :: (MonadState Int m, selfType :< BenchGenTranslateStackOpt m, selfType :< Translate m) => Mixin selfType (Translate m) (BenchGenTranslateStackOpt m)
transBenchStackOpt this super = TBSA {
  toTBSA = super {
  createWrap = \name exp ->
        do (bs,e,t) <- translateM super exp
           let returnType = case t of JClass "java.lang.Integer" -> Just $ J.PrimType $ J.IntT
                                      _ -> Just objClassTy
           let paraType = getParaType t
           empyClosure' <- empyClosure (super) e ""
           stackbody' <- stackMainBody (super) t
           testBody <- BenchGenStack.testfuncBody (up this) paraType
           isTest <- genTest (up this)
           let stackDecl = wrapperClassB name (bs ++ (if (containsNext bs) then [] else [empyClosure'])) returnType (Just $ J.Block $ stackbody') (genParams paraType) (Just (J.Block testBody)) isTest
           return (BenchGenStack.createCUB super [stackDecl], t)
  ,genTest = return True
   }
}
