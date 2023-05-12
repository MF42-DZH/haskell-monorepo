{-# LANGUAGE DeriveFunctor, TupleSections, ScopedTypeVariables, InstanceSigs #-}

module Text.BSP.Monolith where

import Control.Applicative ( Alternative(..) )
import Control.Monad ( void )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Trans.Except ( ExceptT(..), throwE, runExceptT, runExcept )
import Data.ByteString ( ByteString )
import Data.ByteString.Internal ( c2w, w2c )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Functor.Identity ( Identity )
import Data.Maybe ( isNothing )
import Data.Word ( Word8 )

-- Simple result type for these parsers.
-- Use runExceptT and runExcept for extraction of an Either.
type MResult m a = ExceptT [String] m (a, Int)
type Result a    = MResult Identity a
type Parsed a    = Either [String] a

newtype BSPT m a
  = BSPT { runBSPT :: (ByteString, Int) -> MResult m a }
  deriving Functor

type BSP a = BSPT Identity a

runBSP :: BSP a -> (ByteString, Int) -> Result a
runBSP p = runBSPT p

parseT :: Monad m => BSPT m a -> ByteString -> m (Parsed a)
parseT p inp = runExceptT (fst <$> runBSPT p (inp, 0))

parse :: BSP a -> ByteString -> Parsed a
parse p inp = runExcept (fst <$> runBSP p (inp, 0))

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

eof :: Monad m => BSPT m ()
eof = BSPT (\ (inp, off) -> if   isNothing (inp `BS.indexMaybe` off)
                            then return ((), off)
                            else throwE ["EOF not reached yet"])

satisfy :: Monad m => (Word8 -> Bool) -> BSPT m Word8
satisfy p = BSPT $ \ (inp, off) ->
  let chr = inp `BS.indexMaybe` off
  in  case chr of
        Just c  -> if   p c
                   then return (c, off + 1)
                   else throwE $ [ concat [ "predicate does not match character "
                                          , show (fromC8 c)
                                          , " in input "
                                          , show inp
                                          , " at offset "
                                          , show off
                                          ] ]
        Nothing -> throwE ["reached EOF"]

char :: Monad m => Word8 -> BSPT m Word8
char c = BSPT $ \ (inp, off) ->
  let chr = inp `BS.indexMaybe` off
  in  case chr of
        Just c' -> if   c == c'
                   then return (c, off + 1)
                   else throwE $ [ concat [ show (fromC8 c')
                                          , " does not match character "
                                          , show (fromC8 c)
                                          , " in input "
                                          , show inp
                                          , " at offset "
                                          , show off
                                          ] ]
        Nothing -> throwE ["reached EOF"]

string :: Monad m => ByteString -> BSPT m ByteString
string str = foldr (\ c p -> char c *> p) (pure str) (BS.unpack str)

space :: Monad m => BSPT m ()
space = void (satisfy BSI.isSpaceWord8)

spaces :: Monad m => BSPT m ()
spaces = void (many space)

peek :: Monad m => BSPT m ByteString
peek = BSPT (\ (inp, off) -> return (BS.drop off inp, off))

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

skipMany :: Monad m => BSPT m a -> BSPT m ()
skipMany = void . many

skipSome :: Monad m => BSPT m a -> BSPT m ()
skipSome = void . some

sepBy :: Monad m => BSPT m a -> BSPT m sep -> BSPT m [a]
sepBy px psep = sepBy1 px psep ||| pure []

sepBy1 :: Monad m => BSPT m a -> BSPT m sep -> BSPT m [a]
sepBy1 px psep = (:) <$> px <*> many (psep *> px)

optionally :: Monad m => a -> BSPT m a -> BSPT m a
optionally x px = px ||| pure x

optional :: Monad m => BSPT m a -> BSPT m ()
optional px = void px ||| pure ()

between, betwixt :: Monad m => BSPT m l -> BSPT m r -> BSPT m a -> BSPT m a
between pl pr px = pl *> px <* pr
betwixt          = between

choice :: Monad m => [BSPT m a] -> BSPT m a
choice = foldr (|||) empty

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
