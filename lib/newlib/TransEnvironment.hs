{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module TransEnvironment (
    lookupTy,
    extendCtx,
    multiSubst,
    Context,
    oneStep,
    ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Lens.Micro
import Lens.Micro.TH
import Unbound.Generics.LocallyNameless

import Panic
import PrettyPrint
import Syntax


data Context = Ctx {_env :: Tele}

makeLenses ''Context

-- type TcMonad = FreshMT (ReaderT Context (Except T.Text))


-- runTcMonad :: Context -> TcMonad a -> (Either T.Text a)
-- runTcMonad env m = runExcept $ runReaderT (runFreshMT m) env


-- dummyCtx :: Context
-- dummyCtx = Ctx Empty Prog Pos

-- initialEnv :: ClassTag -> Context
-- initialEnv t = Ctx Empty t Pos

lookUpTele :: TmName -> Tele -> Maybe Expr
lookUpTele _ Empty = Nothing
lookUpTele v (Cons rb)
  | v == x = return a
  | otherwise = lookUpTele v t'
  where
    ((x, Embed a), t') = unrebind rb

lookupTyMaybe :: MonadReader Context m => TmName -> m (Maybe Expr)
lookupTyMaybe v = do
  tele <- asks _env
  return (lookUpTele v tele)

lookupTy :: (MonadReader Context m) => TmName -> m Expr
lookupTy v = do
  x <- lookupTyMaybe v
  case x of
    Nothing  -> panic $ "Not in scope: " ++ show v
    Just res -> return res

extendCtx :: (MonadReader Context m) => Tele -> m a -> m a
extendCtx d = local (over env (`appTele` d))


appTele :: Tele -> Tele -> Tele
appTele Empty t2 = t2
appTele (Cons rb) t2 = Cons (rebind p (appTele t1' t2))
  where
    (p, t1') = unrebind rb

multiSubst :: Tele -> Expr -> Expr -> (Tele, Expr)
multiSubst Empty _ e = (Empty, e)
multiSubst (Cons rb) t e = (b', e')
  where
    ((x, _), b) = unrebind rb
    e' = subst x t e
    b' = subst x t b


done :: MonadPlus m => m a
done = mzero

-- | Small step, call-by-value operational semantics
step :: Expr -> MaybeT FreshM Expr
step (Var{}) = done
step (Star) = done
step (Lam{}) = done
step (Pi{}) = done
step (F{}) = done
step (Lit{}) = done
step (Nat) = done
step (App (Lam bnd) t2) = do
  (delta, b) <- unbind bnd
  let (b', e') = multiSubst delta t2 b
  case b' of
    Empty -> return e'
    _     -> return (Lam (bind b' e'))
step (App t1 t2) =
  App <$> step t1 <*> pure t2
  <|> App <$> pure t1 <*> step t2
step (Let bnd) = do
  ((n, Embed e), b) <- unbind bnd
  let n' = name2String n
  elet n' <$> step e <*> pure b <|> pure (subst n e b)
step (U (F _ e)) = return e
step (U e) = U <$> step e
step e@(Mu bnd) = do
  ((n, _), b) <- unbind bnd
  return $ subst n e b
-- TODO: semantics for primop
-- step (PrimOp op (Lit n) (Lit m)) = do
--   let x = evalOp op
--   return (Lit (n `x` m))
step (PrimOp op e1 e2) =
  PrimOp <$> pure op <*> step e1 <*> pure e2
  <|> PrimOp <$> pure op <*> pure e1 <*> step e2

evalOp :: Operation -> Integer -> Integer -> Integer
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mult = (*)

oneStep :: Expr -> Expr
oneStep e = do
  case runFreshM . runMaybeT $ (step e) of
    Nothing -> panic $ "Cannot reduce " ++ showExpr e
    Just e' -> e'
