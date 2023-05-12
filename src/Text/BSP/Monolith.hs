{-# LANGUAGE DeriveFunctor, TupleSections #-}

module Text.BSP.Monolith where

import Control.Monad
import Control.Applicative
import Data.ByteString ( ByteString )
import Data.ByteString.Internal ( c2w, w2c )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Word

-- Simple result type for these parsers.
type Result a = Either [String] (a, Int)

newtype BSP a
  = BSP { runBSP :: (ByteString, Int) -> Result a }
  deriving Functor

instance Applicative BSP where
  pure x = BSP (\ (_, off) -> Right (x, off))

  pf <*> px = BSP $ \ (inp, off) -> do
    (f, off')  <- runBSP pf (inp, off)
    (x, off'') <- runBSP px (inp, off')
    return (f x, off'')

instance Monad BSP where
  return = pure

  px >>= f = BSP $ \ (inp, off) -> do
    (x, off') <- runBSP px (inp, off)
    runBSP (f x) (inp, off')

satisfy :: (Word8 -> Bool) -> BSP Word8
satisfy p = BSP $ \ (inp, off) ->
  let chr = inp `BS.index` off
  in  if   p chr
      then Right (chr, off + 1)
      else Left $ [ concat [ "predicate does not match character "
                           , show (fromC8 chr)
                           , " in input "
                           , show inp
                           , " at offset "
                           , show off
                           ] ]

char :: Word8 -> BSP Word8
char c = BSP $ \ (inp, off) ->
  let chr = inp `BS.index` off
  in  if   c == chr
      then Right (chr, off + 1)
      else Left $ [ concat [ show (fromC8 c)
                           , " does not match character "
                           , show (fromC8 chr)
                           , " in input "
                           , show inp
                           , " at offset "
                           , show off
                           ] ]

string :: ByteString -> BSP ByteString
string str = foldr (\ c p -> char c *> p) (pure str) (BS.unpack str)

chainr1 :: BSP a -> BSP (a -> a -> a) -> BSP a
chainr1 px pop = loop
  where
    loop   = px >>= body
    body x = (($ x) <$> pop <*> loop) ||| pure x

chainr :: BSP a -> BSP (a -> a -> a) -> a -> BSP a
chainr px pop x = chainr1 px pop ||| pure x

chainl1 :: BSP a -> BSP (a -> a -> a) -> BSP a
chainl1 px pop = px >>= loop
  where
    loop x = (($ x) <$> pop <*> px >>= loop) ||| pure x

chainl :: BSP a -> BSP (a -> a -> a) -> a -> BSP a
chainl px pop x = chainl1 px pop ||| pure x

peek :: BSP ByteString
peek = BSP (\ (inp, off) -> Right (BS.drop off inp, off))

failure :: String -> BSP a
failure msg = BSP (const (Left [msg]))

coalesce :: (a -> a -> a) -> Either a b -> Either a b -> Either a b
coalesce _ l@(Right _) _        = l
coalesce _ (Left _) r@(Right _) = r
coalesce op (Left l) (Left r)   = Left (l `op` r)

coalesceErrors :: Either [a] b -> Either [a] b -> Either [a] b
coalesceErrors = coalesce (++)

toC8 :: Char -> Word8
toC8 = c2w

fromC8 :: Word8 -> Char
fromC8 = w2c

infixr 5 |||
(|||) :: BSP a -> BSP a -> BSP a
x ||| y = BSP $ \ (inp, off) ->
  runBSP x (inp, off) `coalesceErrors` runBSP y (inp, off)

instance Alternative BSP where
  empty = failure "No such successful alternative found."

  (<|>) = (|||)
