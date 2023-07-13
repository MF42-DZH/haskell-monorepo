{-# LANGUAGE OverloadedStrings, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Map ( Map )
import qualified Data.Map as M
import Text.BSP.Monolith

infixr 5 :+:
data (f :+: g) a
  = InL (f a)
  | InR (g a)
  deriving Functor

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InR . inj

type StringT = ByteString

newtype Def a = Def (StringT, Int)
  deriving (Functor, Show)

data Exp a
  = Add StringT (Exp a)
  | Sub StringT (Exp a)
  | End StringT (Exp a)
  | Sentinel
  deriving (Functor, Show)

showE :: Exp a -> ByteString
showE (Add l r) = l <> " + " <> showE r
showE (Sub l r) = l <> " - " <> showE r
showE (End n _) = n
showE Sentinel  = ""

newtype Clr a = Clr ()
  deriving (Functor, Show)

newtype Lang f = Lang (f (Lang f))
type DefaultLang = Lang (Def :+: Exp :+: Clr)

injLang :: g :<: f => g (Lang f) -> Lang f
injLang = Lang . inj

type EnvT = StateT (Map StringT Int, Map Int StringT) IO

class Eval d where
  eval :: d -> Bool -> EnvT (Maybe Int)

instance Eval (Def a) where
  eval (Def (k, v)) _ = Nothing <$ modify' (bimap (M.insert k v) (M.insert v k))

showR :: Maybe Int -> EnvT ByteString
showR Nothing  = return "unknown"
showR (Just x) = do
  var <- (M.!? x) . snd <$> get
  return $ case var of
    Nothing -> C8.pack (show x)
    Just v  -> v

instance Eval (Exp a) where
  eval e@(Add l r) pr = do
    l' <- (M.!? l) . fst <$> get
    r' <- eval r False

    let result = ((+) <$> l' <*> r')
    v <- showR result
    lift (when pr (C8.putStrLn (showE e <> " = " <> v)))
    return result
  eval e@(Sub l r) pr = do
    l' <- (M.!? l) . fst <$> get
    r' <- eval r False

    let result = ((-) <$> l' <*> r')
    v <- showR result
    lift (when pr (C8.putStrLn (showE e <> " = " <> v)))
    return result
  eval (End n _) _    = (M.!? n) . fst <$> get
  eval Sentinel _     = return (Just 0)

instance Eval (Clr a) where
  eval (Clr _) _ = Nothing <$ put (M.empty, M.empty)

instance (Eval (f a), Eval (g a)) => Eval ((f :+: g) a) where
  eval (InL f) = eval f
  eval (InR f) = eval f

foldLang :: Functor f => (f a -> a) -> Lang f -> a
foldLang f (Lang t) = f (foldLang f <$> t)

evalDeflang :: DefaultLang -> EnvT (Maybe Int)
evalDeflang = foldLang (`eval` True)

defP :: BSPT IO DefaultLang
defP = injLang <$> do
  () <$ string "def"
  spaces
  ident <- BS.pack <$> some (satisfy (isAlpha . fromC8))
  spaces
  num   <- chainPre (negate <$ char' '-') (read . C8.unpack . BS.pack <$> (some (satisfy (isDigit . fromC8))))
  return (Def (ident, num))

opP :: BSPT IO (Exp a -> Exp a)
opP = do
  ident <- BS.pack <$> some (satisfy (isAlpha . fromC8))
  spaces
  op    <- char' '+' <|> char' '-' <|> char' '='
  return $ case op of
    '+' -> Add ident
    '-' -> Sub ident
    '=' -> End ident
    _   -> error "How'd you get here?"

expP :: BSPT IO DefaultLang
expP = injLang <$> do
  () <$ string "calc"
  expr <- chainPre (spaces *> opP) (pure Sentinel)
  return expr

clrP :: BSPT IO DefaultLang
clrP = injLang (Clr ()) <$ string "clear"

langP :: BSPT IO [DefaultLang]
langP = many (spaces *> choice [defP, expP, clrP])

main :: IO ()
main = do
  inp  <- BS.getContents
  prog <- parseT langP inp
  case prog of
    Left errs -> mapM_ putStrLn errs
    Right p   -> evalStateT (mapM_ evalDeflang p) (M.empty, M.empty)
