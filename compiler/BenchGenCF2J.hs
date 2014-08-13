{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module BenchGenCF2J where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import Inheritance
--import qualified Data.Map as Map
--import qualified Data.Set as Set
import ClosureF
import BaseTransCFJava
import StringPrefixes
import MonadLib

import Language.Java.Pretty
import Text.PrettyPrint.Leijen

data BenchGenTranslateOpt m = TBA {
  toTBA :: Translate m -- supertype is a subtype of Translate (later on at least)
}

instance (:<) (BenchGenTranslateOpt m) (Translate m) where
   up = up . toTBA

instance (:<) (BenchGenTranslateOpt m) (BenchGenTranslateOpt m) where -- reflexivity
  up = id


data BenchGenTranslate m = TB {
  toTB :: Translate m -- supertype is a subtype of Translate (later on at least)
}

instance (:<) (BenchGenTranslate m) (Translate m) where
   up = up . toTB

instance (:<) (BenchGenTranslate m) (BenchGenTranslate m) where -- reflexivity
  up = id


-- nats = 1 : map (+1) nats

testfuncArgType paraType= map (\x -> J.FormalParam [] (J.PrimType J.IntT) False (J.VarId (J.Ident $ "x" ++ show x))) paraType

-- Closure c0 = (Closure) apply();
closureInit id = (J.LocalVars []  closureType--type
	 [J.VarDecl (J.VarId (J.Ident id)) -- name
	 	(Just $ J.InitExp $ (J.Cast closureType) (J.MethodInv $ J.MethodCall (J.Name [(J.Ident "apply")]) []))]) -- init

fieldAcc classId fieldId = (J.PrimaryFieldAccess (J.ExpName (J.Name [(J.Ident classId)])) (J.Ident fieldId))

-- Closure c1 = (Closure) c0.out;
closurePass idl idr = (J.LocalVars [] closureType --type
	 [J.VarDecl (J.VarId (J.Ident idl)) -- name
	 	(Just $ J.InitExp $ (J.Cast closureType) (J.FieldAccess $ (fieldAcc idr "out")))]) -- init

-- c1.x = x;
paraAssign classId paraId = (J.BlockStmt $ J.ExpStmt $ J.Assign (J.FieldLhs $ (fieldAcc classId "x")) (J.EqualA) (J.ExpName $ J.Name [(J.Ident paraId)]))

-- c1.apply();
invokeApply classId = (J.BlockStmt $ J.ExpStmt $ J.MethodInv $ (J.PrimaryMethodCall (J.ExpName $ (J.Name [J.Ident classId])) [] (J.Ident "apply") []))

-- return (Integer) c2.out;
retRes returnType classId = (J.BlockStmt (J.Return $ Just (J.Cast (J.RefType $ (refType returnType)) (J.FieldAccess (fieldAcc classId "out")))))

-- here
testfuncBody paraType = 
	case paraType of
		[] -> []
		x : [] -> [ closureInit c0, paraAssign c0 x0, invokeApply c0, retRes "Integer" c0] 
				where
					c0 = "c" ++ show x
					x0 = "x" ++ show x
					
		x : y : [] ->
			[ closureInit c0, 
			  paraAssign  c0 x0 , 
			  invokeApply c0, 
			  closurePass c1 c0, 
			  paraAssign c1 x1, 
			  invokeApply c1, 
			  retRes "Integer" c1]
			where
				c0 = "c" ++ show x
				x0 = "x" ++ show x
				c1 = "c" ++ show y
				x1 = "x" ++ show y
		_ -> []

getClassDecl className bs ass paraType testfuncBody returnType mainbodyDef = J.ClassTypeDecl (J.ClassDecl [J.Public] (J.Ident className) [] (Nothing) []
	(J.ClassBody [app [J.Static] body returnType "apply" [],
	  app [J.Public, J.Static] (Just (J.Block (testfuncBody paraType))) returnType "test" (testfuncArgType paraType),
      app [J.Public, J.Static] mainbodyDef Nothing "main" mainArgType]))
    where
        body = Just (J.Block (bs ++ ass))

getParaType :: (PCTyp Int) -> [Int]
getParaType tp = case tp of
					CInt -> []
					CTVar _ -> []
					CForall a -> getScopeType a 0
					_ -> []

-- (Scope b t e) -> [Int]
getScopeType (Kind f) n = []
getScopeType (Typ t f) n = 0 : (map (+1) (getScopeType (f ()) 0))
getScopeType _ _= []

benchmarkPackage name = Just (J.PackageDecl (J.Name [(J.Ident name)]))

createCUB this compDef = cu where
   cu = J.CompilationUnit (benchmarkPackage "benchmark") [] ([closureClass this] ++ compDef)



transBench :: (MonadState Int m, selfType :< BenchGenTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (BenchGenTranslate m)
transBench this super = TB {
  toTB = T { 

  translateM = \e -> translateM super e,

  translateScopeM = \e m -> 
             translateScopeM super e m,

  -- here, I guess, you will mainly do the changes: have a look at BaseTransCFJava (and StackTransCFJava) how it's done currently             
  createWrap = \name exp ->
        do (bs,e,t) <- translateM super exp
           let returnType = case t of CInt -> Just $ J.PrimType $ J.IntT
                                      _ -> Just $ objType
           let maybeCastedReturnExp = case t of CInt -> J.Cast boxedIntType e
                                                _ -> J.Cast objType e
           let paraType = getParaType t
           let classDecl = BenchGenCF2J.getClassDecl name bs ([J.BlockStmt (J.Return $ Just maybeCastedReturnExp)]) paraType testfuncBody returnType mainbody
           return (BenchGenCF2J.createCUB super [classDecl], t), 

  closureClass = closureClass super,

  translateApply = translateApply super,

  genApply = genApply super, 

  genRes = genRes super,

  getCvarAss = getCvarAss super
   }
}

