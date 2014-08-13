{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module StackTransCFJava where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import ClosureF
import Inheritance
import qualified Data.Map as Map
import qualified Data.Set as Set
import BaseTransCFJava
import ApplyTransCFJava (countAbs)
import StringPrefixes
import MonadLib

data TranslateStack m = TS {
  toTS :: Translate m -- supertype is a subtype of Translate (later on at least)
  }

instance {-(r :< Translate m) =>-} (:<) (TranslateStack m) (Translate m) where
   up              = up . toTS

instance (:<) (TranslateStack m) (TranslateStack m) where -- reflexivity
  up = id

whileApplyLoop :: String -> J.Ident -> J.Type -> [J.BlockStmt]
whileApplyLoop ctemp tempOut outType = [J.LocalVars [] closureType [J.VarDecl (J.VarId $ J.Ident $ ctemp) (Nothing)],
        J.LocalVars [] outType [J.VarDecl (J.VarId tempOut) (Just (J.InitExp (J.Lit J.Null)))],
        -- this is a hack, because language-java 0.2.x removed J.Paren
        --J.Paren $ J.Assign (J.NameLhs (J.Name [J.Ident ctemp]))
        --        J.EqualA (J.ExpName (J.Name [J.Ident "Next",J.Ident "next"]))
        J.BlockStmt (J.While (J.BinOp (J.ExpName $ J.Name [J.Ident ("(" ++ ctemp ++ " = " ++ "Next.next" ++ ")")])
        J.NotEq (J.Lit J.Null)) (J.StmtBlock (J.Block
        [J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"]))
        J.EqualA (J.Lit J.Null))),
        J.BlockStmt (J.ExpStmt (J.MethodInv (J.MethodCall (J.Name [J.Ident $ ctemp,J.Ident "apply"]) []))),
        J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [tempOut]))
        J.EqualA (J.Cast outType
        (J.ExpName (J.Name [J.Ident $ ctemp,J.Ident "out"])))))])))]

containsNext :: [J.BlockStmt] -> Bool
containsNext l = foldr (||) False $ map (\x -> case x of (J.BlockStmt (J.ExpStmt (J.Assign (
                                                                J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA _))) -> True
                                                         _ -> False) l

-- ad-hoc fix for final-returned expressions in Stack translation
empyClosure outExp = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA 
        (J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] (Just (J.ClassBody [J.MemberDecl (J.MethodDecl 
        [J.Annotation (J.MarkerAnnotation {J.annName = J.Name [J.Ident "Override"]}),J.Public] [] (Just (J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])))) 
        (J.Ident "clone") [] [] (J.MethodBody (Just (J.Block [J.BlockStmt (J.Return (Just (J.Lit J.Null)))])))),
        J.MemberDecl (J.MethodDecl [J.Annotation (J.MarkerAnnotation {J.annName = J.Name [J.Ident "Override"]})] [] Nothing (J.Ident "apply") [] [] (J.MethodBody (Just (J.Block 
        [J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "out"])) J.EqualA outExp))]))))])))))

whileApply :: J.Exp -> String -> J.Ident -> J.Type -> [J.BlockStmt]
whileApply cl ctemp tempOut outType = (J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA (cl))))
         : (whileApplyLoop ctemp tempOut outType)

--e.g. Next.next = x8;
nextApply cl tempOut outType = [J.BlockStmt $ J.ExpStmt $ J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA (cl),
                J.LocalVars [] outType [J.VarDecl (J.VarId tempOut) (Just (J.InitExp (J.Lit J.Null)))]]

stackbody t = 
        applyCall : whileApplyLoop "c" (J.Ident "result") (case t of CJClass ("java.lang.Integer") -> boxedIntType
                                                                     _ -> objType) ++ [
               J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall
    (J.ExpName (J.Name [J.Ident "System.out"])) [] (J.Ident "println") [J.ExpName $ J.Name [J.Ident ("result")]])))]

nextClass = J.ClassTypeDecl (J.ClassDecl [] (J.Ident "Next") [] Nothing [] (J.ClassBody [J.MemberDecl (J.FieldDecl
        [J.Static] (closureType) [J.VarDecl (J.VarId (J.Ident "next")) (Just (J.InitExp (J.Lit J.Null)))])]))

transS :: forall m selfType . (MonadState Int m, MonadReader Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transS this super = TS {toTS = super {  
  translateM = \e -> case e of
       CLam s           -> local (&& False) $ translateM super e
       CFix t s         -> local (&& False) $ translateM super e
       CTApp _ _        -> local (&& False) $ translateM super e
       CFIf0 e1 e2 e3   -> translateIf (up this) (local (|| True) $ translateM (up this) e1) (translateM (up this) e2) (translateM (up this) e3)
       CApp e1 e2       -> translateApply (up this) (local (|| True) $ translateM (up this) e1) (local (|| True) $ translateM (up this) e2)
       otherwise        -> local (|| True) $ translateM super e,

  genApply = \f t x jType -> 
      do (genApplys :: Bool) <- ask 
         (n :: Int) <- get
         put (n+1)
         case x of 
            J.ExpName (J.Name [h]) -> if genApplys then -- relies on translated code!
                                         return (whileApply (J.ExpName (J.Name [f])) ("c" ++ show n) h jType)
                                      else return (nextApply (J.ExpName (J.Name [f])) h jType)
            _ -> error "expected temporary variable name" ,

  genRes = \t s -> return [],
             
  createWrap = \name exp ->
        do (bs,e,t) <- translateM (up this) exp
           let stackDecl = getClassDecl name bs (if (containsNext bs) then [] else [empyClosure e]) Nothing (Just $ J.Block $ stackbody t)
           return (createCUB  (up this :: Translate m) [nextClass,stackDecl], t)
             
  }}

-- Alternative version of transS that interacts with the Apply translation

transSA :: (MonadState Int m, MonadReader Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transSA this super = TS {toTS = (up (transS this super)) {
   genRes = \t s -> if (countAbs t == 0) then return [] else genRes super t s
  }}