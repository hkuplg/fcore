{- |
Module      :  IOEnv
Description :  The main monad used in the type checker.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module IOEnv
  ( IOEnv(..)
  , runIOEnv, evalIOEnv, execIOEnv
  , getEnv, setEnv, localEnv
  ) where

import Control.Monad.State

newtype IOEnv env a = IOEnv { unIOEnv :: StateT env IO a }
  -- Recall:
  -- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }


instance Monad (IOEnv m) where
  return = returnM
  (>>=)  = thenM

returnM :: a -> IOEnv env a
returnM a = IOEnv (StateT (\ env -> return (a, env)))

thenM :: IOEnv env a -> (a -> IOEnv env b) -> IOEnv env b
thenM (IOEnv m) f = IOEnv (StateT (\ env ->
                                     do (r, env') <- runStateT m env
                                        runStateT (unIOEnv (f r)) env'))

instance MonadIO (IOEnv m) where
  liftIO = liftIOM

liftIOM :: IO a -> IOEnv m a
liftIOM = IOEnv . liftIO


runIOEnv :: env -> IOEnv env a -> IO (a, env)
runIOEnv env (IOEnv m) = runStateT m env

evalIOEnv :: env -> IOEnv env a -> IO a
evalIOEnv env = liftM fst . runIOEnv env

execIOEnv :: env -> IOEnv env a -> IO env
execIOEnv env = liftM snd . runIOEnv env


getEnv :: IOEnv env env
getEnv = IOEnv get

setEnv :: env -> IOEnv env ()
setEnv env = IOEnv $ put env

localEnv :: (env -> env) -> IOEnv env a -> IOEnv env a
localEnv f do_this
  = do env <- getEnv
       setEnv (f env)
       r <- do_this
       setEnv env
       return r
