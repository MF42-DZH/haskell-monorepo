{-# LANGUAGE GADTs #-}

module Control.Concurrent.Fake where

import Control.Monad.Trans.Class
import Data.Functor.Identity

-- A restricted version of the Free monad that fakes concurrency between
-- multiple fake threads on an interleaved system.
data ThreadT m a where
  Action :: m (ThreadT m a) -> ThreadT m a
  Result :: a -> ThreadT m a

instance Functor f => Functor (ThreadT f) where
  fmap f (Result x) = Result (f x)
  fmap f (Action x) = Action (fmap (fmap f) x)

instance Functor f => Applicative (ThreadT f) where
  pure = Result

  Result f <*> Result x = Result (f x)
  Result f <*> x        = fmap f x
  Action f <*> x        = Action (fmap (<*> x) f)

instance Functor f => Monad (ThreadT f) where
  return = pure

  Result x >>= f = f x
  Action x >>= f = Action (fmap (>>= f) x)

type Thread = ThreadT Identity

class MonadThread t where
  atomic    :: Monad m => m a -> t m a
  finish    :: Monad m => a -> t m a
  execute   :: Monad m => t m a -> m (t m a)
  getResult :: Monad m => t m a -> m (Maybe a)

instance MonadThread ThreadT where
  atomic x = Action (Result <$> x)

  finish = Result

  execute (Action x) = x
  execute r = pure r

  getResult (Action _) = return Nothing
  getResult (Result x) = return (Just x)

instance MonadTrans ThreadT where
  lift = atomic

roundRobin :: (Monad m, MonadThread t) => [t m a] -> m [a]
roundRobin threads = do
  threads' <- traverse execute threads
  results  <- traverse getResult threads
  case sequence results of
    Nothing -> roundRobin threads'
    Just xs -> pure xs
