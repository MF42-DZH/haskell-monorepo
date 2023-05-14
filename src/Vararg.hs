{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeOperators #-}

module Vararg where

class Constantly x r where
  constantly :: x -> r

instance {-# OVERLAPPABLE #-} (x ~ x') => Constantly x x' where
  constantly = id

instance {-# OVERLAPPING #-} Constantly x b => Constantly x (a -> b) where
  constantly x _ = constantly x
