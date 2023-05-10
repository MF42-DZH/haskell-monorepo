{-# LANGUAGE Rank2Types, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, PolyKinds, TypeFamilies, GADTs, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, Trustworthy #-}

module Refl where

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Kind
import Data.Proxy
import Data.Reflection

-- Newtype wrapper allowing us to dynamically construct Monoids over type a.
-- The s is a rank-2 type which prevents leakage of this implementation
-- (c.f. the ST monad).
newtype M a s
  = M { runM :: a }
  deriving (Show, Eq, Ord)

data Monoid'ish a
  = Monoid'ish { mappend'ish :: a -> a -> a, mempty'ish :: a }

instance Reifies s (Monoid'ish a) => Semigroup (M a s) where
  a <> b = M (mappend'ish (reflect a) (runM a) (runM b))

instance Reifies s (Monoid'ish a) => Monoid (M a s) where
  mappend = (<>)
  mempty  = let x = M (mempty'ish (reflect x)) in x

asProxyOf :: f s -> Proxy s -> f s
asProxyOf x _ = x

-- Allows us to temporarily gain monoid semantics via a given (hopefully)
-- associative function and a (hopefully) neutral element relative to that
-- closed function.
withMonoid :: (a -> a -> a) -> a -> (forall {s :: Type} . Reifies s (Monoid'ish a) => M a s) -> a
withMonoid f z v = reify (Monoid'ish f z) (runM . asProxyOf v)

-- Example: making a monoid from numbers is annoying.
exampleAdd :: (Enum a, Num a) => a
exampleAdd = withMonoid (+) 0 $ mconcat (M <$> [1..10])

newtype O a s
  = O { runO :: a }
  deriving Show

newtype Ord'ish a
  = Ord'ish { compare'ish :: a -> a -> Ordering }

instance Reifies s (Ord'ish a) => Eq (O a s) where
  a == b = compare'ish (reflect a) (runO a) (runO b) == EQ
  
instance Reifies s (Ord'ish a) => Ord (O a s) where
  compare a b = compare'ish (reflect a) (runO a) (runO b)

withOrd :: (a -> a -> Ordering) -> (forall {s :: Type} . Reifies s (Ord'ish a) => O a s) -> a
withOrd cmp v = reify (Ord'ish cmp) (runO . asProxyOf v)

largest :: Integer
largest = withOrd (flip compare) $ minimum (O <$> [3, 7, 9, 2, 4, 0, 1, 5, 7, 8, 6])

class Constantly x r where
  constantly :: x -> r

instance {-# OVERLAPPABLE #-} (x ~ x') => Constantly x x' where
  constantly = id

instance {-# OVERLAPPING #-} Constantly x b => Constantly x (a -> b) where
  constantly x _ = constantly x

testCons :: Int
testCons = constantly 5 '2' [] Nothing (Right "what") () undefined

-- https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
newtype Lift (p :: Type -> Constraint) (a :: Type) (s :: Type)
  = Lift { lower :: a }

class ReifiableConstraint p where
  data Def (p :: Type -> Constraint) (a :: Type) :: Type
  reifiedIns :: Reifies s (Def p a) :- p (Lift p a s)

instance ReifiableConstraint Monoid where
  data Def Monoid a = Monoid { mappend_ :: a -> a -> a, mempty_ :: a }
  reifiedIns = Sub Dict

instance Reifies s (Def Monoid a) => Semigroup (Lift Monoid a s) where
  a <> b = Lift (mappend_ (reflect a) (lower a) (lower b))

instance Reifies s (Def Monoid a) => Monoid (Lift Monoid a s) where
  mappend = (<>)
  mempty  = let x = Lift (mempty_ (reflect x)) in x

instance ReifiableConstraint Ord where
  data Def Ord a = Ord { compare_ :: a -> a -> Ordering }
  reifiedIns = Sub Dict

instance Reifies s (Def Ord a) => Eq (Lift Ord a s) where
  a == b = compare_ (reflect a) (lower a) (lower b) == EQ

instance Reifies s (Def Ord a) => Ord (Lift Ord a s) where
  compare a b = compare_ (reflect a) (lower a) (lower b)

with :: Def p a -> (forall {s :: Type} . Reifies s (Def p a) => Lift p a s) -> a
with d v = reify d (lower . asProxyOf v)

using :: forall p a . ReifiableConstraint p => Def p a -> (p a => a) -> a
using d m = reify d $ \ (_ :: Proxy s) ->
  let replaceProof :: Reifies s (Def p a) :- p a
      replaceProof = trans proof reifiedIns
        where proof = unsafeCoerceConstraint :: p (Lift p a s) :- p a
  in  m \\ replaceProof
