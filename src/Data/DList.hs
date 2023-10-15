{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Data.DList where

import Data.ListClass ( IsList(..) )

newtype DList a
  = DList ([a] -> [a], [a])

infixl 5 :$
pattern (:$) :: ([a] -> [a]) -> [a] -> DList a
pattern f :$ xs = DList (f, xs)

instance IsList DList where
  cons x ~(f :$ xs) = ((x :) . f) :$ xs
  car ~(f :$ xs) = car (f xs)
  cdr ~(f :$ xs) = id :$ cdr (f xs)
  empty = id :$ []
  isEmpty ~(f :$ xs) = isEmpty (f xs)

  fromList = (id :$)
  toList ~(f :$ xs) = f xs
  normalize ~(f :$ xs) = id :$ f xs

  single = (id :$) . pure
  isSingle ~(f :$ xs) = isSingle (f xs)

  append ~(f :$ xs) ~(g :$ ys) = (f xs ++) . g :$ ys
  snoc ~(f :$ xs) x = (++ [x]) . f :$ xs
  size ~(f :$ xs) = size (f xs)
  atIndex ~(f :$ xs) = atIndex (f xs)
  prefixN n ~(f :$ xs) = prefixN n . f :$ xs
  subPrefixN n ~(f :$ xs) = subPrefixN n . f :$ xs

  butLast ~(f :$ xs) = butLast . f :$ xs
  final ~(f :$ xs) = final (f xs)

instance Functor DList where
  fmap f ~(g :$ xs) = (fmap f (g xs) ++) :$ []

  x <$ ~(f :$ xs) = ((x <$ f xs) ++) :$ []

instance Applicative DList where
  pure = single

  ~(f :$ fs) <*> ~(g :$ xs) = ((f fs <*> g xs) ++) :$ []

instance Monad DList where
  return = pure

  ~(f :$ xs) >>= gf = foldr (append . gf) empty (f xs)
