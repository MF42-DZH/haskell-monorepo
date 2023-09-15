module ADFGVX where

import Data.Char ( isAlphaNum, toLower )
import Data.List ( nub, (\\), sort, sortBy, transpose )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )

data PSquare = PSquare
  { _encTable :: Map Char String
  , _decTable :: Map String Char
  }
  deriving Show

-- Guarantees a stable sort for the transposition key.
-- Also adds a potential reversal of the sort.
newtype TransposeKey = TKey { unKey :: (Char, Int) }
  deriving (Show, Eq, Ord)

alphabet, alphabet1 :: String
alphabet  = ['a'..'z'] ++ ['0'..'9']
alphabet1 = ['a'..'z'] ++ ['1'..'9'] ++ ['0']

codeLetters :: String
codeLetters = "adfgvx"

mkPSquare :: Bool -> String -> PSquare
mkPSquare oneFirst alphaKey
  = PSquare (M.fromList (zip uKey cls)) (M.fromList (zip cls uKey))
    where
      cls    = [[x, y] | x <- codeLetters, y <- codeLetters]
      aKey   = nub $ mapMaybe (\ c -> if isAlphaNum c then Just (toLower c) else Nothing) alphaKey
      uAlpha = if oneFirst then alphabet1 else alphabet
      left   = uAlpha \\ aKey
      uKey   = aKey ++ left

-- In this impl, I suppose anything goes?
toKeys :: String -> [TransposeKey]
toKeys str = TKey <$> zip str (iterate (+ 1) 0)

unsortKeys :: [(TransposeKey, a)] -> [(TransposeKey, a)]
unsortKeys = sortBy (\ (TKey (_, x), _) (TKey (_, y), _) -> compare x y)

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = let (group, rest) = splitAt n xs
              in  group : splitN n rest

-- The first argument dictates whether the filler alphabet starts numbers with a 1 (True) or 0 (False).
-- The second argument is the alphanumeric key to generate the Polybius Square.
-- The third argument is the key used for columnar transposition. Ideally, this should be alphabetic.
-- The fourth argument is the text to be encoded / decoded. Anything non alphanumeric will be discarded.
encodeADFGVX, decodeADFGVX :: Bool -> String -> String -> String -> String
encodeADFGVX oneFirst psKey trKey plaintext  =
  let lower            = mapMaybe (\ c -> if isAlphaNum c then Just (toLower c) else Nothing) plaintext
      lowerK           = toLower <$> trKey
      (PSquare encS _) = mkPSquare oneFirst psKey
      columns          = sort $ toKeys lowerK
      preEnc           = lower >>= (encS M.!)
      unshuffled       = zip (cycle [0..length lowerK - 1]) preEnc
      shuffled         = columns >>= (\ col -> snd <$> filter ((== col) . fst) unshuffled) . snd . unKey
  in  shuffled
decodeADFGVX oneFirst psKey trKey ciphertext =
  let lower            = mapMaybe (\ c -> if isAlphaNum c then Just (toLower c) else Nothing) ciphertext
      lowerK           = toLower <$> trKey
      (PSquare _ decS) = mkPSquare oneFirst psKey
      columns          = toKeys lowerK
      lower'           = splitN (length lower `div` length lowerK) lower
      shuffled         = zip (cycle (sort columns)) lower'
      unshuffled       = concat $ transpose $ fmap snd (unsortKeys shuffled)
  in  decode decS unshuffled
    where
      decode _ []             = []
      decode psq (x : y : xs) = (psq M.! [x, y]) : decode psq xs
      decode _ [_]            = error "How'd you get here?"
