{-# LANGUAGE RankNTypes, ScopedTypeVariables, Trustworthy #-}

module Util where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.STRef
import System.Random

shuffleST' :: Int -> [a] -> [a]
shuffleST' seed= fst . shuffleST (mkStdGen seed)

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
