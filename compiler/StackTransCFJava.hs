{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module StackTransCFJava where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import ClosureF
-- import Mixins
import Inheritance
import qualified Data.Map as Map
import qualified Data.Set as Set
import BaseTransCFJava
import StringPrefixes
import MonadLib

data TranslateStack m = TS {
  toTS :: Translate m -- supertype is a subtype of Translate (later on at least)
  }

instance {-(r :< Translate m) =>-} (:<) (TranslateStack m) (Translate m) where
   up              = up . toTS
--   override (TS fm ts) f  = TS (override fm f) ts

instance (:<) (TranslateStack m) (TranslateStack m) where -- reflexivity
  up = id

{-
instance (r :< TranslateStack m) => (:<) r (Translate m) where -- transitivity
  up = up . up
-}

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
        applyCall : whileApplyLoop "c" (J.Ident "result") (case t of CInt -> boxedIntType
                                                                     _ -> objType) ++ [
               J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall
    (J.ExpName (J.Name [J.Ident "System.out"])) [] (J.Ident "println") [J.ExpName $ J.Name [J.Ident ("result")]])))]

nextClass = J.ClassTypeDecl (J.ClassDecl [] (J.Ident "Next") [] Nothing [] (J.ClassBody [J.MemberDecl (J.FieldDecl
        [J.Static] (closureType) [J.VarDecl (J.VarId (J.Ident "next")) (Just (J.InitExp (J.Lit J.Null)))])]))

transS :: (MonadState Int m, MonadReader Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transS this super = TS {
  toTS = T {  translateM = \e -> case e of
       CLam s -> local (&& False) $ translateM super e

       CFix t s -> local (&& False) $ translateM super e

       --CTApp e t -> translateM super e

       CFIf0 e1 e2 e3 ->
                do  n <- get
                    put (n+1)
                    (s1,j1,t1) <- local (|| True) $ translateM (up this) e1
                    let j1' = J.BinOp j1 J.Equal (J.Lit (J.Int 0))
                    genIfBody (up this) e2 e3 j1' s1 n

       CApp e1 e2 ->
               do  (n :: Int) <- get
                   put (n+1)
                   (genApplys :: Bool) <- ask --state before
                   (s1,j1, CForall (Typ t1 g)) <- local (|| True) $ translateM (up this) e1
                   (s2,j2,t2) <-  local (|| True) $ translateM (up this) e2
                   let t    = g ()
                   let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
                   -- cvarass <- getCvarAss t f n j1 j2
                   let cvarass = [ J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp j1))]),
                           J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA j2) )]                                    
                   let genApply = \x jType -> case x of J.ExpName (J.Name [h]) -> if genApplys then 
                                                                (whileApply (J.ExpName (J.Name [f])) ("c" ++ show n) h jType)
                                                                else (nextApply (J.ExpName (J.Name [f])) h jType)
                                                        _ -> error "expected temporary variable name"
                                
                   let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
                   (s3, nje3) <- getS3 t j3 genApply (const []) cvarass
                   return (s1 ++ s2 ++ s3, nje3, scope2ctyp t) -- need to check t1 == t2

       otherwise -> local (|| True) $ translateM super e,

  translateScopeM = \e m -> 
             translateScopeM super e m,
             
  createWrap = \name exp ->
        do (bs,e,t) <- translateM (up this) exp
           let stackDecl = getClassDecl name bs (if (containsNext bs) then [] else [empyClosure e]) Nothing (Just $ J.Block $ stackbody t)
           return (createCUB  super [nextClass,stackDecl], t),

  closureClass = closureClass super             
  }}
