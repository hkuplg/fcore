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


--apply();
methodInvoke id args = (J.BlockStmt $ J.ExpStmt $ J.MethodInv $ J.MethodCall (J.Name [(J.Ident id)]) args)

--Closure c;
localVarDef id = (J.LocalVars [] closureType
				[J.VarDecl (J.VarId (J.Ident id))
				Nothing])

--Object result = null;
resultDef = (J.LocalVars [] objClassTy
			[J.VarDecl (J.VarId (J.Ident "result"))
			(Just $ J.InitExp $ J.Lit J.Null)])

whileApplyLoopB :: String -> J.Ident -> J.Type -> [J.BlockStmt]
whileApplyLoopB ctemp tempOut outType  = 
		[J.BlockStmt (J.While (J.BinOp (J.ExpName $ J.Name [J.Ident ("(" ++ ctemp ++ " = " ++ "hk.hku.cs.f2j.Next.next" ++ ")")])
        J.NotEq (J.Lit J.Null)) (J.StmtBlock (J.Block
        [J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "hk.hku.cs.f2j.Next",J.Ident "next"]))
        J.EqualA (J.Lit J.Null))),
        J.BlockStmt (J.ExpStmt (J.MethodInv (J.MethodCall (J.Name [J.Ident $ ctemp,J.Ident "apply"]) []))),
        J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [tempOut]))
        J.EqualA (J.Cast outType
        (J.ExpName (J.Name [J.Ident $ ctemp,J.Ident "out"])))))])))]
--c = (Closure) result;
--c.x = x;
--Next.next = c;
passClousre from to param = [
	(J.BlockStmt $ J.ExpStmt $ J.Assign (J.NameLhs $ J.Name [(J.Ident to)]) J.EqualA (J.Cast closureType (J.ExpName $ J.Name [(J.Ident from)]))),
	paraAssign to param,
	(J.BlockStmt $ J.ExpStmt $ J.Assign (J.FieldLhs $ (fieldAcc "hk.hku.cs.f2j.Next" "next")) (J.EqualA) (J.ExpName $ J.Name [(J.Ident to)]))
	]

-- TODO: fix name confilc
getClassDecl className bs ass paraType testfuncBody returnType mainbodyDef = J.ClassTypeDecl (J.ClassDecl [J.Public] (J.Ident className) [] (Nothing) []
	(J.ClassBody [J.MemberDecl $ methodDecl [J.Static] Nothing "apply" [] body,
	  J.MemberDecl $ methodDecl [J.Public, J.Static] returnType "test" (methodDeclTemp paraType) (Just (J.Block (testfuncBody paraType))) ,
      J.MemberDecl $ methodDecl [J.Public, J.Static] Nothing "main" mainArgType mainbodyDef]))
    where
        body = Just (J.Block (bs ++ ass))

retResStack returnType id = (J.BlockStmt (J.Return $ Just (J.Cast (classTy returnType) (J.ExpName $ J.Name [(J.Ident id)]))))


testfuncBody paraType = 
	case paraType of
		[] -> []
		x : [] -> [ methodInvoke "apply" [], 
					localVarDef "c",
					resultDef] 
					++ whileApplyLoopB "c" (J.Ident "result") objClassTy
					++ passClousre "result" "c" "x0"
					++ whileApplyLoopB "c" (J.Ident "result") objClassTy
					++ [retResStack "Integer" "result"] 
					
		x : y : [] ->
				[ methodInvoke "apply" [], 
				localVarDef "c",
				resultDef]
				++ whileApplyLoopB "c" (J.Ident "result") objClassTy
				++ passClousre "result" "c" "x0"
				++ whileApplyLoopB "c" (J.Ident "result") objClassTy
				++ passClousre "result" "c" "x1"
				++ whileApplyLoopB "c" (J.Ident "result") objClassTy
				++ [retResStack "Integer" "result"]

		_ -> []


createCUB this compDef = cu where
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
           empyClosure' <- empyClosure (up this) e
           stackbody' <- stackbody (up this) t
           let stackDecl = BenchGenStack.getClassDecl name bs (if (containsNext bs) then [] else [empyClosure']) paraType BenchGenStack.testfuncBody returnType (Just $ J.Block $ stackbody')
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



transBenchStackOpt :: (MonadState Int m, selfType :< BenchGenTranslateStackOpt m, selfType :< Translate m) => Mixin selfType (Translate m) (BenchGenTranslateStackOpt m)
transBenchStackOpt this super = TBSA {
  toTBSA = super { 
  createWrap = \name exp ->
        do (bs,e,t) <- translateM super exp
           let returnType = case t of JClass "java.lang.Integer" -> Just $ J.PrimType $ J.IntT
                                      _ -> Just objClassTy
           let paraType = getParaType t
           empyClosure' <- empyClosure (up this) e
           stackbody' <- stackbody (up this) t
           let stackDecl = BenchGenStack.getClassDecl name bs (if (containsNext bs) then [] else [empyClosure']) paraType BenchGenStack.testfuncBody returnType (Just $ J.Block $ stackbody')
           return (BenchGenStack.createCUB super [stackDecl], t)
           --nextClass
   }
}

