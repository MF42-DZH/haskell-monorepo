{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, RankNTypes, FunctionalDependencies #-}

module Vararg where

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
