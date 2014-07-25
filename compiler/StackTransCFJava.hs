{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module StackTransCFJava where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import ClosureF
-- import Mixins
import Inheritance
import Data.Map as Map
import Data.Set as Set
import BaseTransCFJava
import ApplyTransCFJava
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

whileApply :: J.Exp -> String -> J.Ident -> J.Type -> [J.BlockStmt]
whileApply cl ctemp tempOut outType = (J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA (cl))))
         : (whileApplyLoop ctemp tempOut outType)
--Next.next = x8;
nextApply cl tempOut outType = [J.BlockStmt $ J.ExpStmt $ J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA (cl),
                J.LocalVars [] outType [J.VarDecl (J.VarId tempOut) (Just (J.InitExp (J.Lit J.Null)))]]

-- copy-paste from ApplyOpt, TODO: modularize
transS :: (MonadState Int m, MonadReader Bool m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transS this super = TS {
  toTS = T {  translateM = \e -> case e of
       CLam s -> local (&& False) $ translateM super e

       CFix t s -> local (&& False) $ translateM super e

       --CTApp e t -> translateM super e

       CFIf0 e1 e2 e3 ->
                do  n <- get
                    put (n+1)
                    --(genApplys :: Bool) <- ask --state before
                    (s1,j1,t1) <- local (|| True) $ translateM (up this) (CFPrimOp e1 J.Equal (CFLit 0))
                    genIfBody (up this) e2 e3 j1 s1 n

       CApp e1 e2 ->
               do  (n :: Int) <- get
                   put (n+1)
                   (genApplys :: Bool) <- ask --state before
                   (s1,j1, CForall (Typ t1 g)) <- local (|| True) $ translateM (up this) e1
                   (s2,j2,t2) <-  local (|| True) $ translateM (up this) e2
                   let t    = g ()
                   let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
                   cvarass <- getCvarAss t f n j1 j2
                   let genApply = \x jType -> case x of J.ExpName (J.Name [h]) -> if genApplys then 
                                                                (whileApply (J.ExpName (J.Name [f])) ("c" ++ show n) h jType)
                                                                else (nextApply (J.ExpName (J.Name [f])) h jType)
                                                        _ -> error "expected temporary variable name"
                                
                   let genRes = \x -> []
                   let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
                   s3 <- getS3 t j3 genApply genRes cvarass
                   return (s1 ++ s2 ++ s3, j3, scope2ctyp t) -- need to check t1 == t2

       otherwise -> local (|| True) $ translateM super e,

  translateScopeM = \e m -> 
             translateScopeM super e m,
             
  createWrap = \name exp ->
        do (bs,e,t) <- translateM (up this) exp
           let stackDecl = getClassDecl name bs (if (containsNext bs) then [] else [empyClosure e]) Nothing (Just $ J.Block $ stackbody t)
           return (createCUB  [nextClass,stackDecl], t)             
  }}
