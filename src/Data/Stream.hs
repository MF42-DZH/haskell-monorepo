{-# LANGUAGE DeriveFunctor #-}

module Data.Stream where

import Control.Comonad ( Comonad(..), (=>>) )
import Data.List ( intercalate )
import Vararg

infixr 5 :>

data Stream a = a :> Stream a
  deriving Functor

streamOf :: a -> Stream a
streamOf x = x :> streamOf x

siterate :: (a -> a) -> a -> Stream a
siterate f x = x :> siterate f (f x)

shead :: Stream a -> a
shead (x :> _) = x

stail :: Stream a -> Stream a
stail (_ :> t) = t

stake :: Int -> Stream a -> [a]
stake n (x :> xs)
  | n <= 0    = []
  | otherwise = x : stake (n - 1) xs

instance Show a => Show (Stream a) where
  showsPrec _ xs = let finite = stake 10 xs
                   in  ('{' :) . (intercalate "," (show <$> finite) ++) . (",...}" ++)

sat :: Stream a -> Int -> a
sat (x :> xs) n
  | n < 0     = error "Negative index."
  | n > 0     = sat xs (n - 1)
  | otherwise = x

stails :: Stream a -> Stream (Stream a)
stails xs = xs :> stails (stail xs)

sdiag :: Stream (Stream a) -> Stream a
sdiag (xs :> xxs) = shead xs :> sdiag (stail <$> xxs)

instance PossiblyInfinite Stream where
  infiniteOf = streamOf

instance ParallelAddition Stream where
  padd f (x :> xs) (y :> ys) = f x y :> padd f xs ys

instance Applicative Stream where
  pure = streamOf

  (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

instance Monad Stream where
  return = pure

  xs >>= f = sdiag (f <$> xs)

instance Comonad Stream where
  extract   = shead
  duplicate = stails

toStream :: Comonad c => (c a -> a) -> c a -> Stream (c a)
toStream f = siterate (=>> f)
