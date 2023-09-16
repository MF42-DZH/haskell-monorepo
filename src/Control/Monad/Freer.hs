{-# LANGUAGE GADTs #-}

-- https://okmij.org/ftp/Computation/free-monad.html
module Control.Monad.Freer ( Freer(..), etaF ) where

import Control.Monad

data Freer f a where
  Purer :: a -> Freer f a
  Freer :: f x -> (x -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap f (Purer x)   = Purer (f x)
  fmap f (Freer x g) = Freer x (fmap f . g)

instance Applicative (Freer f) where
  pure            = Purer
  Purer f   <*> x = fmap f x
  Freer y g <*> x = Freer y ((<*> x) . g)

instance Monad (Freer f) where
  return          = pure
  Purer x   >>= f = f x
  Freer x g >>= f = Freer x (g >=> f)

etaF :: f a -> Freer f a
etaF x = Freer x Purer
