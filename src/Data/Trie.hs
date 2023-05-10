{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Data.Trie where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Char
import Data.Functor.Identity
import Data.IORef
import Data.Maybe

class Monad m => MTrie m t where
  emptyM    :: m (t a)

  insertM   :: String -> a -> t a -> m (t a)
  insertM_  :: String -> a -> t a -> m ()
  insertM_ s v = void . insertM s v
  
  deleteM   :: String -> t a -> m (t a)
  deleteM_  :: String -> t a -> m ()
  deleteM_ s = void . deleteM s

  findM     :: String -> t a -> m (Maybe a)

  memberM   :: String -> t a -> m Bool
  memberM s = (isJust <$>) . findM s

  wordListM :: t a -> m [String]

class MTrie Identity t => Trie t where
  empty    :: t a
  empty = runIdentity emptyM

  insert   :: String -> a -> t a -> t a
  insert s v = runIdentity . insertM s v

  delete   :: String -> t a -> t a
  delete s = runIdentity . deleteM s

  find     :: String -> t a -> Maybe a
  find s = runIdentity . findM s

  member   :: String -> t a -> Bool
  member s = runIdentity . memberM s

  wordList :: t a -> [String]
  wordList = runIdentity . wordListM

data IOTrie v
  = IONode (IORef (Maybe v)) (IOArray Char (Maybe (IOTrie v)))

instance MTrie IO IOTrie where
  emptyM = do
    ref <- newIORef Nothing
    arr <- newArray ('A', 'Z') Nothing
    return (IONode ref arr)

  insertM s v t = t <$ go (fmap toUpper s) v t
    where
      go [] v' (IONode ref _) = do
        writeIORef ref (Just v')
      go (s' : ss) v' (IONode _ arr) = do
        child <- readArray arr s'
        case child of
          Nothing  -> do
            newChild <- emptyM
            writeArray arr s' (Just newChild)
            go ss v' newChild
          Just ion -> go ss v' ion

  deleteM s t = t <$ go (fmap toUpper s) t
    where
      go [] (IONode ref _)       = writeIORef ref Nothing
      go (s' : ss) (IONode _ arr) = readArray arr s' >>= mapM_ (go ss)

  findM s = go (fmap toUpper s)
    where
      go [] (IONode ref _)       = readIORef ref
      go (s' : ss) (IONode _ arr) = do
        child <- readArray arr s'
        case child of
          Nothing  -> return Nothing
          Just ion -> go ss ion

  wordListM t = do
    acc <- newIORef []
    go acc "" t
    readIORef acc
    where
      go acc word (IONode ref arr) = do
        item <- readIORef ref
        when (isJust item) (modifyIORef' acc (reverse word :))

        frozen <- assocs <$> freeze arr
        let children = mapMaybe (\ (c, f) -> if isJust f then Just (c, fromJust f) else Nothing) frozen

        forM_ children (\ (c, t') -> go acc (c : word) t')
