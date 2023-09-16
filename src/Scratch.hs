-- {-# LANGUAGE OverloadedStrings #-}

module Scratch where

import Control.Applicative
-- import Data.ByteString ( ByteString )
-- import qualified Data.ByteString as BS
import Data.Char
import Text.BSP.Monolith

signParser :: (Monad m, Num n) => BSPT m (n -> n)
signParser = negate <$ char (toC8 '-')

integerParser :: Monad m => BSPT m Integer
integerParser = chainPre signParser (read . fmap fromC8 <$> some (satisfy (isDigit . fromC8)))
