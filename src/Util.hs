{-# LANGUAGE RankNTypes, ScopedTypeVariables, Trustworthy, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Util where

import Control.Applicative ( (<|>) )
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Bifunctor
import Data.Bool
import Data.STRef
import System.IO ( hFlush, stdout )
import System.Random

traverse_ :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = (() <$) . traverse f

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []       = return Nothing
findM p (x : xs) = (<|>) <$> (bool Nothing (Just x) <$> p x) <*> findM p xs

find :: (a -> Bool) -> [a] -> Maybe a
find _ []       = Nothing
find p (x : xs) = bool Nothing (Just x) (p x) <|> find p xs

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond body = cond >>= (`when` (body >> whileM cond body))

untilM :: Monad m => m Bool -> m () -> m ()
untilM cond action = action >> cond >>= (`unless` untilM cond action)

infixl 4 <~>
(<~>) :: Applicative f => f a -> f b -> f (a, b)
x <~> y = (,) <$> x <*> y

count :: (a -> Bool) -> [a] -> Int
count p = go 0
  where go acc [] = acc
        go acc (x : xs)
          | p x       = go (acc + 1) xs
          | otherwise = go acc xs

shufflePure :: Int -> [a] -> [a]
shufflePure seed xxs
  = snd (iterate (merge . go [] []) (mkStdGen seed, xxs) !! 8)
    where
      go acc1 acc2 (gen, [])     = (gen, acc1, acc2)
      go acc1 acc2 (gen, x : xs) =
        let (n, gen') = randomR (0, 1) gen :: (Int, StdGen)
        in  case n of
          0  -> go (x : acc1) acc2 (gen', xs)
          ~1 -> go acc1 (x : acc2) (gen', xs)
      
      merge (gen, [], acc2)       = (gen, acc2)
      merge (gen, acc1, [])       = (gen, acc1)
      merge (gen, x : xs, y : ys) =
        let (n, gen') = randomR (0, 1) gen :: (Int, StdGen)
        in case n of
          0  -> second (x :) $ merge (gen', xs, y : ys)
          ~1 -> second (y :) $ merge (gen', x : xs, ys)

shuffleST' :: Int -> [a] -> [a]
shuffleST' seed = fst . shuffleST (mkStdGen seed)

shuffleST :: forall x g . RandomGen g => g -> [x] -> ([x], g)
shuffleST gen xs = runST comp
  where
    comp :: forall s . ST s ([x], g)
    comp = do
      cg <- newSTRef gen

      let len   = length xs - 1

      shuffled <- do
        arr <- newListArray (0, len) xs :: ST s (STArray s Int x)

        let step 0 = return ()
            step i = do
              (j, g') <- randomR (0, i) <$> readSTRef cg
              arrI    <- readArray arr i
              arrJ    <- readArray arr j

              writeArray arr i arrJ
              writeArray arr j arrI
              writeSTRef cg g'

              step (i - 1)

        step len
        return arr

      cg'   <- readSTRef cg
      final <- freeze shuffled

      return (elems final, cg')

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x : xs) =
  let (f, t) = partition p xs
  in  if   p x
      then (f, x : t)
      else (x : f, t)

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine
