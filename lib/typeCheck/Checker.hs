{-
Infrastructure for typechecking.
-}
{-# LANGUAGE RecordWildCards #-}
module Checker where

import TypeErrors
import Control.Monad.Except
import Src
import qualified JvmTypeQuery
import JavaUtils
import IOEnv

import Control.Monad.Except
import qualified Data.Map  as Map
import qualified Data.Set  as Set

type Checker a = ExceptT LTypeErrorExpr (IOEnv CheckerState) a

getCheckerState :: Checker CheckerState
getCheckerState = lift getEnv

setCheckerState :: CheckerState -> Checker ()
setCheckerState tc_env = lift $ setEnv tc_env

getTypeContext :: Checker TypeContext
getTypeContext = liftM checkerTypeContext getCheckerState

getValueContext :: Checker ValueContext
getValueContext = liftM checkerValueContext getCheckerState

getModuleContext :: Checker ModuleContext
getModuleContext = liftM checkerModuleContext getCheckerState

getTypeServer :: Checker JvmTypeQuery.Connection
getTypeServer = liftM checkerTypeServer getCheckerState

getMemoizedJavaClasses :: Checker (Set.Set ClassName)
getMemoizedJavaClasses = liftM checkerMemoizedJavaClasses getCheckerState

memoizeJavaClass :: ClassName -> Checker ()
memoizeJavaClass c
  = do CheckerState{..} <- getCheckerState
       memoized_java_classes <- getMemoizedJavaClasses
       setCheckerState CheckerState{ checkerMemoizedJavaClasses = c `Set.insert` memoized_java_classes, ..}

withLocalTVars :: [(ReadId, (Kind, TypeValue))] -> Checker a -> Checker a
withLocalTVars tvars do_this
  = do delta <- getTypeContext
       let delta' = Map.fromList tvars `Map.union` delta
                -- `Map.fromList` is right-biased and `Map.union` is left-biased.
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerTypeContext = delta', ..}
       r <- do_this
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerTypeContext = delta, ..}
       return r

withLocalVars :: [(ReadId, Type)]-> Checker a -> Checker a
withLocalVars vars do_this
  = do gamma <- getValueContext
       let gamma' = Map.fromList vars `Map.union` gamma
                -- `Map.fromList` is right-biased and `Map.union` is left-biased.
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerValueContext = gamma', ..}
       r <- do_this
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerValueContext = gamma, ..}
       return r

withLocalMVars :: [(ReadId, ModuleMapInfo)]-> Checker a -> Checker a
withLocalMVars vars do_this
  = do sigma <- getModuleContext
       let sigma' = Map.fromList vars `Map.union` sigma
                -- `Map.fromList` is right-biased and `Map.union` is left-biased.
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerModuleContext = sigma', ..}
       r <- do_this
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerModuleContext = sigma, ..}
       return r


data CheckerState
  = CheckerState
  { checkerTypeContext  :: TypeContext
  , checkerValueContext :: ValueContext
  , checkerModuleContext :: ModuleContext
  , checkerTypeServer   :: JvmTypeQuery.Connection
  , checkerMemoizedJavaClasses :: Set.Set ClassName -- Memoized Java class names
  }

mkInitCheckerState :: ModuleContext -> JvmTypeQuery.Connection -> CheckerState
mkInitCheckerState module_ctxt type_server
  = CheckerState
  { checkerTypeContext     = Map.empty
  , checkerValueContext    = Map.empty
  , checkerModuleContext    = module_ctxt
  , checkerTypeServer   = type_server
  , checkerMemoizedJavaClasses = Set.empty
  }

-- Temporary hack for REPL
mkInitCheckerStateWithEnv :: ValueContext -> JvmTypeQuery.Connection -> CheckerState
mkInitCheckerStateWithEnv value_ctxt type_server
  = CheckerState
  { checkerTypeContext     = Map.empty
  , checkerValueContext    = value_ctxt
  , checkerModuleContext    = Map.empty
  , checkerTypeServer   = type_server
  , checkerMemoizedJavaClasses = Set.empty
  }

lookupVar :: Name -> Checker (Maybe Type)
lookupVar x
  = do valueCtxt <- getValueContext
       return (Map.lookup x valueCtxt)

lookupTVarKind :: Name -> Checker (Maybe Kind)
lookupTVarKind a
 = do typeCtxt <- getTypeContext
      return (fmap fst (Map.lookup a typeCtxt))

subtype :: Type -> Type -> Checker Bool
subtype t1 t2
  = do typeCtxt <- getTypeContext
       return (Src.subtype typeCtxt t1 t2)
