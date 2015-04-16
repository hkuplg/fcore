{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XRankNTypes -XTypeOperators  -XOverlappingInstances #-}

module MonadLib (module MonadLib, module Control.Monad, module Data.Monoid) where

import Control.Monad
import Data.Monoid
import Control.Applicative (Applicative(..), Alternative(..))

-- Interfaces for various types of effects:

class (Monad m) => MonadState s m where
    get :: m s
    put :: s -> m ()

class Monad m => MonadReader r m where
    ask   :: m r
    local :: (r -> r) -> m a  -> m a
    reader :: (r -> a) -> m a
    reader f = do
      r <- ask
      return (f r)

class (Monoid w, Monad m) => MonadWriter w m where
    tell   :: w -> m ()
    listen :: m a -> m (a, w)
    pass   :: m (a, w -> w) -> m a

-- Monad Transformers

class MonadTrans t where
    lift :: Monad m => m a -> t m a

-- Reader Monad/Monad Transformers

asks :: MonadReader r m => (r -> a) -> m a
asks = reader

{-
instance MonadReader r ((->) r) where
    ask       = id
    local f m = m . f
    reader    = id
-}

newtype Reader r a = Reader {runReader :: r -> a}

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

instance Functor (Reader r) where
    fmap f m = Reader $ \r -> f (runReader m r)

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

instance Applicative (Reader r) where
    pure  = return
    (<*>) = ap

instance MonadReader r (Reader r) where
    ask       = Reader id
    local f m = Reader $ runReader m . f

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Monad m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r -> do
        a <- runReaderT m r
        return (f a)

instance (Monad m) => Applicative (ReaderT r m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k  = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = ReaderT $ \_ -> fail msg

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = ReaderT $ \_ -> mzero
    m `mplus` n = ReaderT $ \r -> runReaderT m r `mplus` runReaderT n r

instance (Monad m, MonadPlus m) => Alternative (ReaderT r m) where
    (<|>) = mplus
    empty = mzero

instance (Monad m) => MonadReader r (ReaderT r m) where
    ask       = ReaderT return
    local f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadReader r m => MonadReader r (ReaderT t m) where
    ask       = ReaderT (\_ -> ask)
    local f m = ReaderT (\r -> local f (runReaderT m r))

-- State monad/monad transformers

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
    s <- get
    put (f s)

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
    s <- get
    return (f s)

newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s  -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f

instance Functor (State s) where
    fmap f m = State $ \s -> case runState m s of
                                 (a, s') -> (f a, s')

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> case runState m s of
                                 (a, s') -> runState (k a) s'

instance Applicative (State s) where
    pure  = return
    (<*>) = ap

instance MonadState s (State s) where
    get   = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (_, s') <- runStateT m s
    return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

instance (Monad m) => Functor (StateT s m) where
    fmap f m = StateT $ \s -> do
        (x, s') <- runStateT m s
        return (f x, s')

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance (Monad m) => Applicative (StateT s m) where
    pure  = return
    (<*>) = ap

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero       = StateT $ \_ -> mzero
    m `mplus` n = StateT $ \s -> runStateT m s `mplus` runStateT n s

instance (Monad m, MonadPlus m) => Alternative (StateT s m) where
    (<|>) = mplus
    empty = mzero

instance (Monad m) => MonadState s (StateT s m) where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance MonadState s m => MonadState s (StateT t m) where
    get   = StateT $ \s -> liftM (\x -> (x,s)) get
    put s = StateT $ \s1 -> liftM (\x -> (x,s1)) (put s)

-- Writer monad/monad transformer

listens :: (MonadWriter w m) => (w -> b) -> m a -> m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

censor :: (MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do
    a <- m
    return (a, f)

newtype Writer w a = Writer { runWriter :: (a, w) }

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f m = Writer $ f (runWriter m)

instance Functor (Writer w) where
    fmap f m = Writer $ case runWriter m of
                            (a, w) -> (f a, w)

instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    m >>= k  = Writer $ case runWriter m of
                            (a, w) -> case runWriter (k a) of
                                (b, w') -> (b, w `mappend` w')

instance (Monoid w) => Applicative (Writer w) where
    pure  = return
    (<*>) = ap

instance (Monoid w) => MonadWriter w (Writer w) where
    tell   w = Writer ((), w)
    listen m = Writer $ case runWriter m of
                            (a, w) -> ((a, w), w)
    pass   m = Writer $ case runWriter m of
                            ((a, f), w) -> (a, f w)

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
    (_, w) <- runWriterT m
    return w

mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

instance (Monad m) => Functor (WriterT w m) where
    fmap f m = WriterT $ do
        (a, w) <- runWriterT m
        return (f a, w)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= k  = WriterT $ do
        (a, w)  <- runWriterT m
        (b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    fail msg = WriterT $ fail msg

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
    pure  = return
    (<*>) = ap

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero       = WriterT mzero
    m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n

instance (Monoid w, Monad m, MonadPlus m) => Alternative (WriterT w m) where
    (<|>) = mplus
    empty = mzero

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    tell   w = WriterT $ return ((), w)
    listen m = WriterT $ do
        (a, w) <- runWriterT m
        return ((a, w), w)
    pass   m = WriterT $ do
        ((a, f), w) <- runWriterT m
        return (a, f w)

instance (MonadWriter w m, Monoid t) => MonadWriter w (WriterT t m) where
    tell   w = WriterT $ liftM (\x -> (x,mempty)) $ tell w
    listen m = WriterT $ liftM (\((x,y),z) -> ((x,z),y)) $ listen (runWriterT m)
    pass   m = WriterT $ pass $ liftM (\(((x,f),t)) -> (((x,t),f))) $ runWriterT m

-- Transformers:

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \_ -> m

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        a <- m
        return (a, mempty)

-- Interaction between effects:

instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

instance (MonadWriter w m) => MonadWriter w (ReaderT r m) where
    tell     = lift . tell
    listen m = ReaderT $ \w -> listen (runReaderT m w)
    pass   m = ReaderT $ \w -> pass   (runReaderT m w)

instance (MonadReader r m) => MonadReader r (StateT s m) where
    ask       = lift ask
    local f m = StateT $ \s -> local f (runStateT m s)

instance (MonadWriter w m) => MonadWriter w (StateT s m) where
    tell     = lift . tell
    listen m = StateT $ \s -> do
        ((a, s'), w) <- listen (runStateT m s)
        return ((a, w), s')
    pass   m = StateT $ \s -> pass $ do
        ((a, f), s') <- runStateT m s
        return ((a, s'), f)

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
    ask       = lift ask
    local f m = WriterT $ local f (runWriterT m)

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
    get = lift get
    put = lift . put

-- We need views to support multiple monads of the same type

data n :-> m = View {
  fromV :: forall a . n a -> m a,
  toV :: forall a . m a -> n a
  }

-- auxiliary definitions with views

getV :: MonadState s n => (n :-> m) -> m s
getV view = fromV view get

putV :: MonadState s n => (n :-> m) -> s -> m ()
putV view x = fromV view (put x)

localV :: MonadReader r n => (n :-> m) -> (r -> r) -> m a -> m a
localV view f ma = fromV view $ local f (toV view ma)

