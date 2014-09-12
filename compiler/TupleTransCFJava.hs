{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module TupleTransCFJava where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import Inheritance
--import qualified Data.Map as Map
import qualified Data.Set as Set
import ClosureF
import BaseTransCFJava hiding (chooseCastBox, getS3, javaType)
import StringPrefixes
import MonadLib
import Control.Monad


tuple2Type = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "hk.hku.cs.f2j.tuples.Tuple2",[])]))
initTuple2 tempvarstr n j = initStuff tempvarstr n j tuple2Type

chooseCastBox (JClass c)       = (initClassCast c, javaClassType c)
chooseCastBox (Forall _)       = (initClosure,closureType)
chooseCastBox (TupleType [t])  = chooseCastBox t -- optimization for tuples of size 1
chooseCastBox (TupleType [t1, t2]) = (initTuple2, tuple2Type)
chooseCastBox (TupleType _)    = (initObjArray,objArrayType)
--chooseCastBox (CListOf t)       = (initPrimList, primListType)
chooseCastBox _                 = (initObj,objType)

javaType (JClass c)      = javaClassType c
javaType (Forall _)      = closureType
javaType (TupleType [t]) = javaType t -- optimization for tuples of size 1
javaType (TupleType [t1, t2]) = tuple2Type
javaType (TupleType _)   = objArrayType
--javaType (CListOf t)      = primListType
javaType _                = objType

getS3 this f t j3 cvarass =
  do (n :: Int) <- get
     put (n+1)
     let (cast,typ) = chooseCastBox (scope2ctyp t)
     apply <- genApply this f t (var (tempvarstr ++ show n)) typ
     rest <- genRes this t [cast tempvarstr n j3]                                                           
     let r = cvarass ++ apply ++ rest
     return (r, var (tempvarstr ++ show n))   


data TupleTranslate m = TT {
  toTT :: Translate m -- supertype is a subtype of Translate (later on at least)
}

instance (:<) (TupleTranslate m) (Translate m) where
   up = up . toTT

instance (:<) (TupleTranslate m) (TupleTranslate m) where -- reflexivity
  up = id

newTuple2Class id fst snd = J.InstanceCreation [] (J.ClassType [((J.Ident id), [])]) [fst,snd] Nothing


transTuple :: (MonadState Int m, selfType :< TupleTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (TupleTranslate m)
transTuple this super = TT {
  toTT = super {

  translateM = \e -> case e of
  	-- an optimization for 2 element tuple
	Tuple [e1,e2] ->
	    do  (s1,j1,t1) <- translateM super e1
	        (s2,j2,t2) <- translateM super e2
	        (n :: Int) <- get
	        put (n+1)
	        return (s1 ++ s2 ++ [assignVar (localvarstr ++ show n) (newTuple2Class "hk.hku.cs.f2j.tuples.Tuple2" j1 j2) (JClass "hk.hku.cs.f2j.tuples.Tuple2")], 
	        	var (localvarstr ++ show n), TupleType [t1,t2])
	Proj i e   ->
	    do (s1,j1,t) <- translateM super e
	       case t of 
	       	  TupleType [t1, t2] -> 
	       	      case i of 
	                  1 -> 
	       	    	    let fj = J.FieldAccess (J.PrimaryFieldAccess j1 (J.Ident "fst")) 
	       	    	    in 
	       	    	    return (s1, J.Cast (javaType t1) fj, t1)
	       	    	  2 -> 
	       	    	    let fj = J.FieldAccess (J.PrimaryFieldAccess j1 (J.Ident "snd"))
	            	    in
	            	    return (s1, J.Cast (javaType t2) fj, t2)
	            	  _ -> error "tuple index out of range"
	_          -> translateM super e,

  translateApply = \m1 m2 -> 
   do  (n :: Int) <- get
       put (n+1)
       (s1,j1, Forall (Type t1 g)) <- m1
       (s2,j2,t2) <- m2
       let t    = g ()
       let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
       cvarass <- getCvarAss (up this) t f j1 j2
       let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
       (s3, nje3) <- getS3 (up this) f t j3 cvarass
       return (s1 ++ s2 ++ s3, nje3, scope2ctyp t)

  }
}
