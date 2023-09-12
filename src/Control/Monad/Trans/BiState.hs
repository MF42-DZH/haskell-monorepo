{-# LANGUAGE DeriveFunctor, RecursiveDo, TupleSections #-}
{-# LANGUAGE RankNTypes, QuantifiedConstraints, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Control.Monad.Trans.BiState where

import Control.Monad.Fix ( MonadFix(..) )
import Data.Functor.Identity ( Identity(..) )

newtype BiStateT bw fw m a
  = BiStateT { runBiStateT :: (bw, fw) -> m (a, (bw, fw)) }
  deriving Functor

type BiState bw fw a
  = BiStateT bw fw Identity a

instance MonadFix m => Applicative (BiStateT bw fw m) where
  pure x = BiStateT (\ s -> return (x, s))

  (BiStateT bsf) <*> (BiStateT bsx) = BiStateT $ \ s -> do
    rec
      (f, s')  <- bsf (fst s'', snd s)
      (x, s'') <- bsx (fst s, snd s')
    return (f x, (fst s', snd s''))

instance MonadFix m => Monad (BiStateT bw fw m) where
  return = pure

  (BiStateT bsx) >>= f
    = BiStateT $ \ s -> do
      rec
        (a, s')  <- bsx (fst s'', snd s)
        (b, s'') <- runBiStateT (f a) (fst s, snd s')
      return (b, (fst s', snd s''))

instance MonadFix m => MonadFix (BiStateT bw fw m) where
  mfix f = BiStateT (\ s -> mfix (\ ~(x, _) -> runBiStateT (f x) s))

liftB :: Functor m => m a -> BiStateT bw fw m a
liftB ma = BiStateT (\ s -> (, s) <$> ma)

put :: MonadFix m => fw -> BiStateT bw fw m ()
tup :: MonadFix m => bw -> BiStateT bw fw m ()

put x = BiStateT (\ s -> return ((), (fst s, x)))
tup y = BiStateT (\ s -> return ((), (y, snd s)))

get :: MonadFix m => BiStateT bw fw m fw
teg :: MonadFix m => BiStateT bw fw m bw

get = BiStateT (\ s -> return (snd s, s))
teg = BiStateT (\ s -> return (fst s, s))

modify :: MonadFix m => (fw -> fw) -> BiStateT bw fw m ()
yfidom :: MonadFix m => (bw -> bw) -> BiStateT bw fw m ()

modify f = f <$> get >>= put
yfidom f = f <$> teg >>= tup

evalBiStateT :: MonadFix m => BiStateT bw fw m a -> (bw, fw) -> m a
evalBiStateT bs s = fst <$> runBiStateT bs s

execBiStateT :: MonadFix m => BiStateT bw fw m a -> (bw, fw) -> m (bw, fw)
execBiStateT bs s = snd <$> runBiStateT bs s

runBiState :: BiState bw fw a -> (bw, fw) -> (a, (bw, fw))
runBiState bs s = runIdentity (runBiStateT bs s)

evalBiState :: BiState bw fw a -> (bw, fw) -> a
evalBiState = (fst .) . runBiState

execBiState :: BiState bw fw a -> (bw, fw) -> (bw, fw)
execBiState = (snd .) . runBiState
