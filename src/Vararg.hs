{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE UndecidableInstances, RankNTypes, FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables #-}

module Vararg where

import Data.Array ( Array )
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Sequence ( Seq(..), (|>) )
import qualified Data.Sequence as SQ
import Data.Set ( Set )
import qualified Data.Set as St
import Data.Void ( Void )

class Constantly x r where
  constantly :: x -> r

instance {-# OVERLAPPABLE #-} (x ~ x') => Constantly x x' where
  constantly = id

instance {-# OVERLAPPING #-} Constantly x b => Constantly x (a -> b) where
  constantly x _ = constantly x

class MZipI fs rs | fs -> rs where
  collapse :: fs -> rs

class PossiblyInfinite f where
  -- Analogous to Applicative's "pure", but must return an infinite structure.
  -- Think "repeat" for lists, but generalised.
  infiniteOf :: a -> f a

class ParallelAddition f where
  -- This is analogous to zipWith, but generalised for all containers.
  -- Note: liftA2 (or something equivalent with ap / <*>) is invalid!
  padd :: (a -> b -> c) -> f a -> f b -> f c

instance PossiblyInfinite [] where
  infiniteOf = repeat

instance ParallelAddition [] where
  padd = zipWith

instance {-# OVERLAPPING #-} (PossiblyInfinite f, ParallelAddition f, MZipI (f fs) rs) => MZipI (f (a -> fs)) (f a -> rs) where
  collapse fs as = collapse (padd ($) fs as)

instance {-# OVERLAPPABLE #-} (PossiblyInfinite f, ParallelAddition f, f r ~ rs) => MZipI (f r) rs where
  collapse = id

multizip :: forall f fs rs . (PossiblyInfinite f, ParallelAddition f, MZipI (f fs) rs) => fs -> rs
multizip f = collapse (infiniteOf f :: f fs)

-- Emulates Clojure's map function for lists.
-- A specialization of multizip for lists.
multimap :: MZipI [f] rs => f -> rs
multimap f = collapse (repeat f)

class ToColl a t | t -> a where
  coll' :: Seq a -> a -> t

instance ToColl a [a] where
  coll' acc = F.toList . (acc |>)

instance ToColl a (Seq a) where
  coll' acc = (acc |>)

instance Ord a => ToColl a (Set a) where
  coll' acc x = F.foldl' (flip St.insert) St.empty (acc |> x)

instance ToColl a (Array Int a) where
  coll' acc x = A.listArray (0, SQ.length acc) (F.toList (acc |> x))

instance ToColl a r => ToColl a (a -> r) where
  coll' acc x = coll' (acc |> x)

coll :: ToColl a r => a -> r
coll = coll' SQ.empty

infixr 5 :$:

data ArgList f r t where
  FN    :: ArgList r r Void
  (:$:) :: b -> ArgList f r t -> ArgList (b -> f) r (ArgList f r t)

apply :: f -> ArgList f r a -> r
apply f FN        = f
apply f (x :$: t) = apply (f x) t
