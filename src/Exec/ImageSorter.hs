{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad ( forM_, when )
import Data.List ( isSuffixOf, stripPrefix )
import Data.Maybe ( fromMaybe )
import Codec.Picture
import System.Directory
import System.Environment
import System.Exit

isFinished :: Int -> DynamicImage -> Bool
isFinished threshold img = any ((> threshold) . ($ img)) [dynWidth, dynHeight]

dynWidth, dynHeight :: DynamicImage -> Int
dynWidth  = dynamicMap imageWidth
dynHeight = dynamicMap imageHeight

getImagesFrom :: FilePath -> IO [FilePath]
getImagesFrom path
  = fmap ((path ++) . ('/' :))
  . filter (\ p -> isSuffixOf ".png" p || isSuffixOf ".jpg" p)
  <$> listDirectory path

processedDir, toProcessDir :: FilePath
processedDir = "processed"
toProcessDir = "to_process"

notSingleton :: [a] -> Bool
notSingleton [_] = False
notSingleton _   = True

main :: IO ()
main = do
  args <- getArgs
  when (notSingleton args) (putStrLn "Put one numeric argument." >> exitFailure)

  processed <- listDirectory processedDir
  !_        <- forM_ processed (\ pth -> do
    let src = processedDir ++ '/' : pth
        dst = toProcessDir ++ '/' : pth
    !x <- renamePath src dst
    !_ <- putStrLn ("Moving processed image " ++ src ++ " to " ++ dst ++ ".")
    return x)

  toProcess <- listDirectory toProcessDir
  !u        <- forM_ toProcess (\ pth -> do
    let pth' = toProcessDir ++ '/' : pth
    let dst  = processedDir ++ '/' : fromMaybe pth' (stripPrefix (toProcessDir ++ "/") pth')

    Right img <- readImage pth'
    when (isFinished (read (head args)) img) (do
      !x <- renamePath pth' dst
      !_ <- putStrLn ("Moving finished image " ++ pth' ++ " to " ++ dst ++ ".")
      return x))
  
  putStrLn "Done!\n"
  return u
