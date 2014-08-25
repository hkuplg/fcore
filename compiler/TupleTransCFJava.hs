{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module BenchGenCF2J where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import Inheritance
--import qualified Data.Map as Map
import qualified Data.Set as Set
import ClosureF
import BaseTransCFJava
import StringPrefixes
import MonadLib
import Control.Monad


chooseCastBox (CJClass c)       = (initClassCast c, javaClassType c)
chooseCastBox (CForall _)       = (initClosure,closureType)
chooseCastBox (CTupleType [t])  = chooseCastBox t -- optimization for tuples of size 1
chooseCastBox (CTupleType _)    = (initObjArray,objArrayType)
--chooseCastBox (CListOf t)       = (initPrimList, primListType)
chooseCastBox _                 = (initObj,objType)

javaType (CJClass c)      = javaClassType c
javaType (CForall _)      = closureType
javaType (CTupleType [t]) = javaType t -- optimization for tuples of size 1
javaType (CTupleType _)   = objArrayType
--javaType (CListOf t)      = primListType
javaType _                = objType



data TupleTranslate m = TT {
  toTT :: Translate m -- supertype is a subtype of Translate (later on at least)
}

instance (:<) (TupleTranslate m) (Translate m) where
   up = up . toTB

instance (:<) (TupleTranslate m) (TupleTranslate m) where -- reflexivity
  up = id

newTuple2Class id fst snd = [J.InstanceCreation [] [((J.Ident id), [fst, snd])] [] Nothing]


transTuple :: (MonadState Int m, selfType :< TupleTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (TupleTranslate m)
transTuple this super = TT {
  toTT = super {

  translateM = \e -> case e of
  	-- an optimization for 2 element tuple
	CFTuple [e1,e2] ->
       do  (s1,j1,t1) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           (n :: Int) <- get
           put (n+1)
       	   return (s1 ++ s2 ++ [assignVar (localvarstr ++ show n) (newTuple2Class "hk.hku.cs.f2j.Tuple2" j1 j2) (CJCLass "hk.hku.cs.f2j.Tuple2")], 
       	   var (localvarstr ++ show n),CTupleType [t1,t2])


    --CFProj i e ->
    --   do (s1,j1,t) <- translateM this e
    --      case t of 
    --         -- A simple optimization for tuples of size 1 (DOES NOT NEED TO BE FORMALIZED) 
    --         CTupleType [t] -> return (s1,j1,t)
    --         -- An alternative tranlation of tuple2
    --         CTupleType [t1, t2] -> 
    --         	let 
    --         -- otherwise: (not optimized)
    --         CTupleType ts  -> 
    --           let fj = J.ArrayAccess (J.ArrayIndex j1 (J.Lit (J.Int $ toInteger (i-1)))) 
    --           in return (s1, J.Cast (javaType (ts!!(i-1))) fj, ts!!(i-1))
    --         otherwise -> error "expected tuple type" 

    _ -> translateM super e

  }
}
