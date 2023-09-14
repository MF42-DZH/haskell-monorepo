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

-- The class of all infinite-double-sided structures.
class TapeLike t where
  -- Law: fwd . rev == id == rev . fwd
  fwd :: t a -> t a
  rev :: t a -> t a

  -- Law: (writeHead x) . readHead != x (if x is not already at the tape head).
  -- Law: readHead . (writeHead x) == x
  readHead  :: t a -> a
  writeHead :: a -> t a -> t a

newtype Tape a = Tape (Stream a, a, Stream a)

tapeOf :: a -> Tape a
tapeOf x = Tape (streamOf x, x, streamOf x)

instance TapeLike Tape where
  fwd (Tape (rev', head', f :> fwd')) = Tape (head' :> rev', f, fwd')
  rev (Tape (r :> rev', head', fwd')) = Tape (rev', r, head' :> fwd')
  readHead (Tape (_, x, _))           = x
  writeHead x (Tape (r, _, f))        = Tape (r, x, f)

instance Show a => Show (Tape a) where
  showsPrec _ (Tape (r, x, f))
    = ('{' :) . (r' ++) . ('|' :) . (show x ++) . ('|' :) . (f' ++) . ('}' :)
    where
      r' = intercalate "," $ show <$> reverse (stake 10 r)
      f' = intercalate "," $ show <$> stake 10 f
