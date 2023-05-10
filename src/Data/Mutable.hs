{-# LANGUAGE RankNTypes, BangPatterns, MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module Data.Mutable where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.ST
import Control.Monad.STM
import Data.IORef
import Data.Kind
import Data.STRef

class Monad m => MRef m ref where
  mnew     :: a -> m (ref a)

  mread    :: ref a -> m a
  mwrite   :: ref a -> a -> m ()

  mmodify  :: ref a -> (a -> a) -> m ()
  mmodify ref f = mread ref >>= mwrite ref . f

  mmodify' :: ref a -> (a -> a) -> m () -- Usually the strict version of mmodify.
  mmodify' !ref f = do
    !x <- mread ref
    let !y = f x
    !u <- mwrite ref y
    return u

instance MRef IO IORef where
  mnew     = newIORef
  mread    = readIORef
  mwrite   = writeIORef
  mmodify  = modifyIORef
  mmodify' = modifyIORef'

instance MRef IO MVar where
  mnew          = newMVar
  mread         = readMVar
  mwrite var x  = modifyMVar_ var (const (return x))
  mmodify var f = modifyMVar_ var (return . f)

instance forall s . MRef (ST s) (STRef s) where
  mnew     = newSTRef
  mread    = readSTRef
  mwrite   = writeSTRef
  mmodify  = modifySTRef
  mmodify' = modifySTRef'

instance MRef STM TVar where
  mnew     = newTVar
  mread    = readTVar
  mwrite   = writeTVar
  mmodify  = modifyTVar
  mmodify' = modifyTVar'

instance MRef STM TMVar where
  mnew     = newTMVar
  mread    = readTMVar
  mwrite   = writeTMVar

newtype AMPair (m :: Type -> Type) ref a b
  = AMPair (ref a, ref b)

-- Convenience aliases for use with MPair.
type IORefPair   = AMPair IO IORef
type MVarPair    = AMPair IO MVar
type STRefPair s = AMPair (ST s) (STRef s)
type TVarPair    = AMPair STM TVar
type TMVarPair   = AMPair STM TMVar

class (Monad m, MRef m ref) => MPair m ref p where
  -- You most likely don't need to use these three functions.
  constructor :: ref a -> ref b -> p m ref a b
  fstRef      :: p m ref a b -> ref a
  sndRef      :: p m ref a b -> ref b

  mnewPair :: a -> b -> m (p m ref a b)
  mnewPair x y = constructor <$> mnew x <*> mnew y

  mreadFst :: p m ref a b -> m a
  mreadFst = mread . fstRef

  mreadSnd :: p m ref a b -> m b
  mreadSnd = mread . sndRef

  mwriteBi :: p m ref a b -> a -> b -> m ()
  mwriteBi p x y = mwriteFst p x >> mwriteSnd p y

  mwriteFst :: p m ref a b -> a -> m ()
  mwriteFst p = mwrite (fstRef p)

  mwriteSnd :: p m ref a b -> b -> m ()
  mwriteSnd p = mwrite (sndRef p)

  mmodifyBi :: p m ref a b -> (a -> a) -> (b -> b) -> m ()
  mmodifyBi p f g = mmodifyFst p f >> mmodifySnd p g

  mmodifyBi' :: p m ref a b -> (a -> a) -> (b -> b) -> m ()
  mmodifyBi' !p f g = mmodifyFst' p f >> mmodifySnd' p g

  mmodifyFst :: p m ref a b -> (a -> a) -> m ()
  mmodifyFst p = mmodify (fstRef p)

  mmodifySnd :: p m ref a b -> (b -> b) -> m ()
  mmodifySnd p = mmodify (sndRef p)

  mmodifyFst' :: p m ref a b -> (a -> a) -> m ()
  mmodifyFst' p = mmodify' (fstRef p)

  mmodifySnd' :: p m ref a b -> (b -> b) -> m ()
  mmodifySnd' p = mmodify' (sndRef p)

instance (Monad m, MRef m ref) => MPair m ref AMPair where
  constructor            = curry AMPair
  fstRef (AMPair (f, _)) = f
  sndRef (AMPair (_, s)) = s
