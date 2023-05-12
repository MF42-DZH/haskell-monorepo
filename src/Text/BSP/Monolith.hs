{-# LANGUAGE DeriveFunctor, TupleSections, ScopedTypeVariables, InstanceSigs #-}

module Text.BSP.Monolith where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString ( ByteString )
import Data.ByteString.Internal ( c2w, w2c )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Functor.Identity
import Data.Word

-- Simple result type for these parsers.
-- Use runExceptT and runExcept for extraction of an Either.
type MResult m a = ExceptT [String] m (a, Int)
type Result a    = MResult Identity a

newtype BSPT m a
  = BSPT { runBSPT :: (ByteString, Int) -> MResult m a }
  deriving Functor

type BSP a = BSPT Identity a

runBSP :: BSP a -> (ByteString, Int) -> Result a
runBSP p = runBSPT p

instance Monad m => Applicative (BSPT m) where
  pure x = BSPT (\ (_, off) -> return (x, off))

  (<*>) :: BSPT m (a -> b) -> BSPT m a -> BSPT m b
  pf <*> px = BSPT $ \ (inp, off) -> do
    (f, off')  <- runBSPT pf (inp, off)
    (x, off'') <- runBSPT px (inp, off')
    return (f x, off'')

instance Monad m => Monad (BSPT m) where
  return = pure

  px >>= f = BSPT $ \ (inp, off) -> do
    (x, off') <- runBSPT px (inp, off)
    runBSPT (f x) (inp, off')

instance MonadTrans BSPT where
  lift m = BSPT (\ (_, off) -> lift ((,off) <$> m))

satisfy :: Monad m => (Word8 -> Bool) -> BSPT m Word8
satisfy p = BSPT $ \ (inp, off) ->
  let chr = inp `BS.index` off
  in  if   p chr
      then return (chr, off + 1)
      else throwE $ [ concat [ "predicate does not match character "
                             , show (fromC8 chr)
                             , " in input "
                             , show inp
                             , " at offset "
                             , show off
                             ] ]

char :: Monad m => Word8 -> BSPT m Word8
char c = BSPT $ \ (inp, off) ->
  let chr = inp `BS.index` off
  in  if   c == chr
      then return (chr, off + 1)
      else throwE $ [ concat [ show (fromC8 c)
                             , " does not match character "
                             , show (fromC8 chr)
                             , " in input "
                             , show inp
                             , " at offset "
                             , show off
                             ] ]

string :: Monad m => ByteString -> BSPT m ByteString
string str = foldr (\ c p -> char c *> p) (pure str) (BS.unpack str)

chainr1 :: Monad m => BSPT m a -> BSPT m (a -> a -> a) -> BSPT m a
chainr1 px pop = loop
  where
    loop   = px >>= body
    body x = (($ x) <$> pop <*> loop) ||| pure x

chainr :: Monad m => BSPT m a -> BSPT m (a -> a -> a) -> a -> BSPT m a
chainr px pop x = chainr1 px pop ||| pure x

chainl1 :: Monad m => BSPT m a -> BSPT m (a -> a -> a) -> BSPT m a
chainl1 px pop = px >>= loop
  where
    loop x = (($ x) <$> pop <*> px >>= loop) ||| pure x

chainl :: Monad m => BSPT m a -> BSPT m (a -> a -> a) -> a -> BSPT m a
chainl px pop x = chainl1 px pop ||| pure x

peek :: Monad m => BSPT m ByteString
peek = BSPT (\ (inp, off) -> return (BS.drop off inp, off))

failure :: Monad m => String -> BSPT m a
failure msg = BSPT (const (throwE [msg]))

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
(|||) :: Monad m => BSPT m a -> BSPT m a -> BSPT m a
x ||| y = BSPT $ \ (inp, off) -> ExceptT $
  coalesceErrors <$> runExceptT (runBSPT x (inp, off)) <*> runExceptT (runBSPT y (inp, off))

instance Monad m => Alternative (BSPT m) where
  empty = failure "No such successful alternative found."

  (<|>) = (|||)
