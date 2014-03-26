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

data NaiveTranslate = NT {
  toTr :: Translate
  }

transUN :: Open NaiveTranslate
transUN this = NT { toTr = T {
  translateM = \e -> translateM (toTr this) e,
  translateScopeM = \e -> case e of 
      Typ t f ->
        do  n <- get
            put (n+2)
            (s,je,t1) <- translateScopeM (toTr this) (f (n+1,t))
            let f    = J.Ident ("x" ++ show n) -- use a fresh variable
            let self = J.Ident ("x" ++ show (n+1)) -- use another fresh variable
            let currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId self) (Just (J.InitExp J.This))]
            let outputAssignment = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  je))
            let cvar = J.LocalVars [] closureType [J.VarDecl (J.VarId f) 
                                                (Just (J.InitExp 
                                                    (jexp [currentInitialDeclaration] (Just (J.Block (s ++ [outputAssignment]))))
                                                    )
                                                )]
            return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise -> translateScopeM (toTr this) e
    }
   }