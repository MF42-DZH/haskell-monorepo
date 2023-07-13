{-# LANGUAGE DeriveFunctor, TupleSections, ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

-- A one-file monolithic parser-combinator module for ByteStrings.
-- Intended for performance and minimal memory usage in competitive programming.
-- If only simple tokenisation is needed, use Text.ScannerGeneric instead.
module Text.BSP.Monolith where

import Control.Applicative ( Alternative(..), (<**>) )
import Control.Monad ( void )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Control.Monad.Trans.Except ( ExceptT(..), throwE, runExceptT, runExcept )
import Data.Bifunctor ( Bifunctor(..) )
import Data.Bool ( bool )
import Data.ByteString ( ByteString )
import Data.ByteString.Internal ( c2w, w2c )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Char8 as C8
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
  pure x = BSPT (\ ~(_, off) -> return (x, off))

  (<*>) :: BSPT m (a -> b) -> BSPT m a -> BSPT m b
  pf <*> px = BSPT $ \ ~(inp, off) -> do
    (f, off')  <- runBSPT pf (inp, off)
    (x, off'') <- runBSPT px (inp, off')
    return (f x, off'')

instance Monad m => Monad (BSPT m) where
  return = pure

  px >>= f = BSPT $ \ ~(inp, off) -> do
    (x, off') <- runBSPT px (inp, off)
    runBSPT (f x) (inp, off')

instance MonadTrans BSPT where
  lift m = BSPT (\ ~(_, off) -> lift ((,off) <$> m))

-- Compatibility version of indexMaybe for lower versions of bytestring.
infixl 9 !?
(!?) :: ByteString -> Int -> Maybe Word8
bs !? i
  | i >= 0 && i < BS.length bs = Just (BS.index bs i)
  | otherwise                  = Nothing

eof :: Monad m => BSPT m ()
eof = BSPT (\ ~(inp, off) -> if   isNothing (inp !? off)
                             then return ((), off)
                             else throwE ["EOF not reached yet"])

satisfy :: Monad m => (Word8 -> Bool) -> BSPT m Word8
satisfy p = BSPT $ \ ~(inp, off) ->
  let chr = inp !? off
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
char c = BSPT $ \ ~(inp, off) ->
  let chr = inp !? off
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

char' :: Monad m => Char -> BSPT m Char
char' c = fromC8 <$> char (toC8 c)

string :: Monad m => ByteString -> BSPT m ByteString
string str = foldr (\ c p -> char c *> p) (pure str) (BS.unpack str)

string' :: Monad m => String -> BSPT m String
string' str = str <$ string (C8.pack str)

space :: Monad m => BSPT m ()
space = void (satisfy BSI.isSpaceWord8)

spaces :: Monad m => BSPT m ()
spaces = void (many space)

line :: Monad m => BSPT m ByteString
line = BSPT $ \ ~(inp, off) -> do
  let lfc8 = toC8 '\n'
  (r, off') <- runBSPT (some (satisfy (/= lfc8)) ||| ([] <$ char lfc8)) (inp, off)
  return (if null r then "" else BS.take (off' - off) (BS.drop off inp), off' + 1)

peek :: Monad m => BSPT m ByteString
peek = BSPT (\ ~(inp, off) -> return (BS.drop off inp, off))

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

chainPre :: Monad m => BSPT m (a -> a) -> BSPT m a -> BSPT m a
chainPre pop px = foldr (.) id <$> many pop <*> px

chainPost :: Monad m => BSPT m (a -> a) -> BSPT m a -> BSPT m a
chainPost pop px = px <**> (foldr (.) id <$> many pop)

pfoldr :: Monad m => (a -> b -> b) -> BSPT m a -> b -> BSPT m b
pfoldr f p z = (foldr f z <$> many p) ||| pure z

pfoldl :: Monad m => (b -> a -> b) -> BSPT m a -> b -> BSPT m b
pfoldl f p z = (foldl f z <$> many p) ||| pure z

-- For non-skip versions of many and some, use Control.Applicative.
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

branch :: Monad m => BSPT m (Either a b) -> BSPT m (a -> c) -> BSPT m (b -> c) -> BSPT m c
branch ab ac bc = (\ e a b -> either a b e) <$> ab <*> ac <*> bc

ifP :: Monad m => BSPT m Bool -> BSPT m a -> BSPT m a -> BSPT m a
ifP cond pt pf = (\ c t f -> bool f t c) <$> cond <*> pt <*> pf

newtype ExceptT' m e a
  = ExceptT' { unExceptT' :: ExceptT e m a }
  deriving Functor

instance Monad m => Bifunctor (ExceptT' m) where
  first f (ExceptT' (ExceptT c)) = ExceptT' $ ExceptT $ c >>= return . \ case
    Left l  -> Left (f l)
    Right r -> Right r

  second f (ExceptT' (ExceptT c)) = ExceptT' $ ExceptT $ c >>= return . \ case
    Left l  -> Left l
    Right r -> Right (f r)

hijack :: Monad m => BSPT m a -> [String] -> BSPT m a
hijack p newMessages = BSPT $ \ ~(inp, off) ->
  unExceptT' $ first (const newMessages) (ExceptT' (runBSPT p (inp, off)))

label :: Monad m => BSPT m a -> String -> BSPT m a
label p name = BSPT $ \ ~(inp, off) ->
  unExceptT' $ first (concat ["--", name, "--"] :) (ExceptT' (runBSPT p (inp, off)))

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

-- It is recommended to use this over <|> if you expect the LHS of your parsers to
-- be successful more often.
infixr 5 |||
(|||) :: Monad m => BSPT m a -> BSPT m a -> BSPT m a
x ||| y = BSPT $ \ ~(inp, off) -> ExceptT $
  coalesceErrors <$> runExceptT (runBSPT x (inp, off)) <*> runExceptT (runBSPT y (inp, off))

instance Monad m => Alternative (BSPT m) where
  empty = failure "No such successful alternative found."

  (<|>) = (|||)
