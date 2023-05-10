{-# LANGUAGE FlexibleInstances, InstanceSigs #-} -- Lower GHC version compatibility.

-- Source: https://byorgey.wordpress.com/2019/05/22/competitive-programming-in-haskell-scanner/
-- Modified to be a proper monad transformer, and allow generic token parsing.
module Text.ScannerGeneric where

import Control.Monad ( replicateM, replicateM_, unless, void )
import Control.Monad.Trans.State.Lazy ( StateT, evalStateT, get, put )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Maybe ( fromJust )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- The base ScannerT type.
type ScannerT s m = StateT [s] m
type Scanner s = ScannerT s Identity

runScannerT :: Monad m => (s -> [s]) -> ScannerT s m a -> s -> m a
runScannerT tkf scn = evalStateT scn . tkf

runScannerT_ :: Monad m => (s -> [s]) -> ScannerT s m a -> s -> m ()
runScannerT_ tkf scn = void . runScannerT tkf scn

runScanner :: (s -> [s]) -> Scanner s a -> s -> a
runScanner tkf scn = runIdentity . runScannerT tkf scn

nextToken :: Monad m => ScannerT s m s
nextToken = get >>= \ ss -> put (tail ss) >> return (head ss)

skipToken :: Monad m => ScannerT s m ()
skipToken = nextToken >>= const (return ())

skipTokens :: Monad m => Int -> ScannerT s m ()
skipTokens = (`replicateM_` skipToken)

nextWith :: Monad m => (s -> a) -> ScannerT s m a
nextWith reader = reader <$> nextToken

scanNumberOf :: Monad m => (s -> Int) -> ScannerT s m a -> ScannerT s m [a]
scanNumberOf nextInt scn = nextWith nextInt >>= (`replicateM` scn)

scanUntil :: Monad m => (a -> Bool) -> ScannerT s m a -> ScannerT s m ()
scanUntil p scn = scn >>= (`unless` scanUntil p scn) . p

scanMany :: Monad m => ScannerT s m a -> ScannerT s m [a]
scanMany scn = get >>= \ xs -> if null xs then return [] else (:) <$> scn <*> scanMany scn

scanN :: Monad m => Int -> ScannerT s m a -> ScannerT s m [a]
scanN = replicateM

-- The type class used for automatically handling defaults for ScannerT.
class Scannable s where
  -- What is this Scannable's equivalent of String's "read"?
  defaultParse :: Read a => s -> a

  -- What is this Scannable's equivalent of String's "words"?
  defaultWords :: s -> [s]

  -- Extra parsers for int and integer, in case they are faster.
  parseInt :: s -> Int
  parseInt = defaultParse

  parseInteger :: s -> Integer
  parseInteger = defaultParse

  -- Run scanner with default "words"-like function.
  runScannerT' :: Monad m => ScannerT s m a -> s -> m a
  runScannerT' = runScannerT defaultWords

  -- Run pure scanner with default "words"-like function.
  runScanner' :: Scanner s a -> s -> a
  runScanner' = runScanner defaultWords

  -- Same as runScannerT', but discards the result.
  runScannerT'_ :: Monad m => ScannerT s m a -> s -> m ()
  runScannerT'_ scn = void . runScannerT' scn

  -- A scanner that allows one to read any Read instance.
  nextReadable :: (Monad m, Read a) => ScannerT s m a
  nextReadable = nextWith defaultParse

  -- Scan number, then scan that number of items, without needing to specify a parser.
  scanNumberOf' :: Monad m => ScannerT s m a -> ScannerT s m [a]
  scanNumberOf' = scanNumberOf parseInt

-- String specialisation of ScannerT.
type StrScannerT m a = ScannerT String m a
type StrScanner a = StrScannerT Identity a

instance Scannable String where
  defaultParse :: Read a => String -> a
  defaultParse = read

  defaultWords :: String -> [String]
  defaultWords = words

-- Strict Bytestring specialisation of ScannerT.
type ByteStringScannerT m a = ScannerT ByteString m a
type ByteStringScanner a = ByteStringScannerT Identity a

instance Scannable ByteString where
  defaultParse :: Read a => ByteString -> a
  defaultParse = read . C.unpack

  defaultWords :: ByteString -> [ByteString]
  defaultWords = C.words

  parseInt :: ByteString -> Int
  parseInt = fst . fromJust . C.readInt

  parseInteger :: ByteString -> Integer
  parseInteger = fst . fromJust . C.readInteger

-- Lazy Bytestring specialisation of ScannerT.
type LazyByteString = LB.ByteString
type LazyByteStringScannerT m a = ScannerT LazyByteString m a
type LazyByteStringScanner a = LazyByteStringScannerT Identity a

instance Scannable LazyByteString where
  defaultParse :: Read a => LazyByteString -> a
  defaultParse = read . LC.unpack

  defaultWords :: LazyByteString -> [LazyByteString]
  defaultWords = LC.words

  parseInt :: LazyByteString -> Int
  parseInt = fst . fromJust . LC.readInt

  parseInteger :: LazyByteString -> Integer
  parseInteger = fst . fromJust . LC.readInteger

-- Strict Text specialisation of ScannerT.
type TextScannerT m a = ScannerT Text m a
type TextScanner a = TextScannerT Identity a

instance Scannable Text where
  defaultParse :: Read a => Text -> a
  defaultParse = read . T.unpack

  defaultWords :: Text -> [Text]
  defaultWords = T.words

-- Lazy Text specialisation of ScannerT.
type LazyText = LT.Text
type LTextScannerT m a = ScannerT LazyText m a
type LTextScanner a = LTextScannerT Identity a

instance Scannable LazyText where
  defaultParse :: Read a => LazyText -> a
  defaultParse = read . LT.unpack

  defaultWords :: LazyText -> [LazyText]
  defaultWords = LT.words
