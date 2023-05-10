{-# LANGUAGE DeriveFunctor, RecursiveDo, TupleSections #-}
{-# LANGUAGE RankNTypes, QuantifiedConstraints, FlexibleContexts, UndecidableInstances #-}

module Control.Monad.Trans.RState where

import Control.Monad.Fix ( MonadFix(..) )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Data.Functor.Identity ( Identity(..) )

newtype RStateT s m a
  = RStateT { runRStateT :: s -> m (a, s) }
  deriving Functor

type RState s a = RStateT s Identity a

instance MonadFix m => Applicative (RStateT s m) where
  pure a = RStateT $ \ s -> return (a, s)

  (RStateT rsf) <*> (RStateT rsx)
    = RStateT $ \ s -> do
      rec
        (f, past)   <- rsf future
        (x, future) <- rsx s
      return (f x, past)

instance MonadFix m => MonadFix (RStateT s m) where
  mfix f = RStateT $ \ s -> mfix $ \ ~(a, _) -> runRStateT (f a) s

instance MonadFix m => Monad (RStateT s m) where
  return = pure

  (RStateT rst) >>= rstgf
    = RStateT $ \ s -> do
      rec
        (a, past)   <- rst future
        (b, future) <- runRStateT (rstgf a) s
      return (b, past)

instance (forall s' m . Monad (RStateT s' m)) => MonadTrans (RStateT s) where
  lift ma = RStateT $ \ s -> (, s) <$> ma

-- Put, Get, Modify.
yfidom :: Monad m => (s -> s) -> RStateT s m ()
yfidom f = RStateT $ \ s -> return ((), f s)

tup :: Monad m => s -> RStateT s m ()
tup = yfidom . const

teg :: Monad m => RStateT s m s
teg = RStateT $ \ s -> return (s, s)

-- RStateT utilities.
rStateT :: Monad m => (s -> (a, s)) -> RStateT s m a
rStateT f = RStateT $ \ s -> let r = f s in return r

evalRStateT :: Monad m => RStateT s m a -> s -> m a
evalRStateT rs s = fst <$> runRStateT rs s

execRStateT :: Monad m => RStateT s m a -> s -> m s
execRStateT rs s = snd <$> runRStateT rs s

-- RState utilities.
rState :: (s -> (a, s)) -> RState s a
rState f = RStateT $ \ s -> let r = f s in return r

runRState :: RState s a -> s -> (a, s)
runRState rs s = runIdentity $ runRStateT rs s

evalRState :: RState s a -> s -> a
evalRState = (fst .) . runRState

execRState :: RState s a -> s -> s
execRState = (snd .) . runRState
