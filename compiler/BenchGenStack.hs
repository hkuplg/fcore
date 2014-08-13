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
import StringPrefixes
import MonadLib

import Language.Java.Pretty
import Text.PrettyPrint.Leijen

data BenchGenTranslateStack m = TBS {
  toTBS :: Translate m -- supertype is a subtype of Translate (later on at least)
}

instance (:<) (BenchGenTranslateStack m) (Translate m) where
   up = up . toTBS

instance (:<) (BenchGenTranslateStack m) (BenchGenTranslateStack m) where -- reflexivity
  up = id


--apply();
methodInvoke id args = (J.BlockStmt $ J.ExpStmt $ J.MethodInv $ J.MethodCall (J.Name [(J.Ident id)]) args)

--Closure c;
localVarDef id = (J.LocalVars [] closureType
				[J.VarDecl (J.VarId (J.Ident id))
				Nothing])

--Object result = null;
resultDef = (J.LocalVars [] closureType
			[J.VarDecl (J.VarId (J.Ident "result"))
			(Just $ J.InitExp $ J.Lit J.Null)])

--c = (Closure) result;
--c.x = x;
--Next.next = c;
passClousre from to param = [
	(J.BlockStmt $ J.ExpStmt $ J.Assign (J.NameLhs $ J.Name [(J.Ident to)]) J.EqualA (J.Cast closureType (J.ExpName $ J.Name [(J.Ident from)]))),
	paraAssign to param,
	(J.BlockStmt $ J.ExpStmt $ J.Assign (J.FieldLhs $ (fieldAcc "Next" "next")) (J.EqualA) (J.ExpName $ J.Name [(J.Ident to)]))
	]


--while ((c = Next.next) != null)
--{
--  Next.next = null;
--  c.apply();
--  result = (Object) c.out;
--}	   
--return (Integer) result;

-- Closure c0 = (Closure) apply();

-- Closure c1 = (Closure) c0.out;

testfuncBody paraType = 
	case paraType of
		[] -> []
		x : [] -> [ methodInvoke "apply" [], 
					localVarDef "c" ] 
					++ whileApplyLoop "c" (J.Ident "result") objType
					++ passClousre "result" "c" x0
					++ whileApplyLoop "c" (J.Ident "result") objType
					++ [retRes "Integer" "result"] 
				where
					x0 = "x" ++ show x
					
		x : y : [] ->
				[ methodInvoke "apply" [], 
				localVarDef "c" ]
				++ whileApplyLoop "c" (J.Ident "result") objType
				++ passClousre "result" "c" x0
				++ whileApplyLoop "c" (J.Ident "result") objType
				++ passClousre "result" "c" x1
				++ whileApplyLoop "c" (J.Ident "result") objType
				++ [retRes "Integer" "result"]
				where
					x0 = "x" ++ show x
					x1 = "x" ++ show y
		_ -> []


createCUB this compDef = cu where
   cu = J.CompilationUnit (benchmarkPackage "benchmark") [] ([closureClass this] ++ compDef)



transBenchStack :: (MonadState Int m, selfType :< BenchGenTranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (BenchGenTranslateStack m)
transBenchStack this super = TBS {
  toTBS = T { 

  translateM = translateM super,

  translateScopeM = translateScopeM super,

  -- here, I guess, you will mainly do the changes: have a look at BaseTransCFJava (and StackTransCFJava) how it's done currently             
  --createWrap = \name exp ->
  --      do (bs,e,t) <- translateM super exp
  --         let returnType = case t of CInt -> Just $ J.PrimType $ J.IntT
  --                                    _ -> Just $ objType
  --         let maybeCastedReturnExp = case t of CInt -> J.Cast boxedIntType e
  --                                              _ -> J.Cast objType e
  --         let paraType = getParaType t
  --         let classDecl = BenchGenStack.getClassDecl name bs ([J.BlockStmt (J.Return $ Just maybeCastedReturnExp)]) paraType testfuncBody returnType mainbody
  --         return (BenchGenStack.createCUB super [classDecl], t), 


  createWrap = \name exp ->
        do (bs,e,t) <- translateM super exp
           let returnType = case t of CInt -> Just $ J.PrimType $ J.IntT
                                      _ -> Just $ objType
           let maybeCastedReturnExp = case t of CInt -> J.Cast boxedIntType e
                                                _ -> J.Cast objType e
           let paraType = getParaType t
           let classDecl = BenchGenCF2J.getClassDecl name bs ([J.BlockStmt (J.Return $ Just maybeCastedReturnExp)]) paraType BenchGenStack.testfuncBody returnType mainbody
           return (BenchGenStack.createCUB super [classDecl], t), 

  closureClass = closureClass super,

  translateApply = translateApply super,

  genApply = genApply super, 

  genRes = genRes super,

  getCvarAss = getCvarAss super

   }
}

