{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE UndecidableInstances, RankNTypes, FunctionalDependencies #-}

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

instance {-# OVERLAPPING #-} MZipI [fs] rs => MZipI [a -> fs] ([a] -> rs) where
  collapse fs as = collapse (zipWith ($) fs as)

instance {-# OVERLAPPABLE #-} ([r] ~ rs) => MZipI [r] rs where
  collapse = id

multizip :: MZipI [f] rs => f -> rs
multizip f = collapse (repeat f)

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
