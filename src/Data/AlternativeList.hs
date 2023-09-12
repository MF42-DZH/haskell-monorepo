{-# LANGUAGE GADTs, TypeSynonymInstances, RankNTypes #-}

module Data.AlternativeList where

import Data.Bool ( bool )
import Data.Either

newtype Fix f = Fix { unFix :: f (Fix f) }

newtype AList x f = AList { _alInternal :: Bool -> Either f x }
type AListF x = Fix (AList x)

aSingle :: x -> AListF x
aSingle x = Fix (AList (bool undefined (Right x)))

aCons :: x -> AListF x -> AListF x
aCons x xs = Fix (AList (bool (Left xs) (Right x)))

aHead :: AListF x -> x
aHead = fromRight undefined . ($ True) . _alInternal . unFix

aTail :: AListF x -> AListF x
aTail = fromLeft undefined . ($ False) . _alInternal . unFix
