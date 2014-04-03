{-# OPTIONS -XFlexibleContexts #-}

module UberNaiveTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures 
import Prelude hiding (init)
import Debug.Trace
import Data.List hiding (init)

import Control.Monad.State

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins

import TransCFJava 

data NaiveTranslate m = NT {
  toTr :: Translate m
  }

transUN :: MonadState Int m => Open (NaiveTranslate m)
transUN this = NT { toTr = T {
  translateM = \e -> translateM (toTr this) e,
  translateScopeM = \e m -> case e of 
      Typ t g -> 
        do  n <- get
            let f    = J.Ident ("x" ++ show n) -- use a fresh variable              
            case m of -- Consider refactoring later?
              Just (i,t') | TransCFJava.last (g (Right i,t')) ->
                do  put (n+1)
                    let self = J.Ident ("x" ++ show i)
                    (s,je,t1) <- translateScopeM (toTr this) (g (Left i,t)) m
                    let cvar = standardTranslation je s self f
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
              otherwise -> 
                do  put (n+2)
                    let self = J.Ident ("x" ++ show (n+1)) -- use another fresh variable              
                    (s,je,t1) <- translateScopeM (toTr this) (g (Left (n+1),t)) m
                    let cvar = standardTranslation je s self f
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise -> translateScopeM (toTr this) e m
    }
   }