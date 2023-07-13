{-# LANGUAGE ApplicativeDo, RankNTypes, GADTs, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, FlexibleContexts #-}

module Data.WadC where

import Control.Applicative.Free.Fast
import Control.Monad
import Data.Comp.Ops
import Data.List
import Util

helloWorld :: WadC ()
helloWorld = WadC (include "standard.h") $ do
  def "main" [] $ do
    call "box" (int <$> [0, 128, 160, 256, 256])
    call "movestep" (coords 32 32)
    call0 "thing"
    pure ()
  pure ()

complex :: WadC ()
complex = WadC (includeAll ["standard.h", "monsters.h", "pickups.h"]) $ do
  boring
  boring2

  def "main" [] $ do
    call "pushpop" $ pure $ do
      call "movestep" (coords 32 32)
      call0 "thing"
      pure ()
    call "boring" $ pure $ choice (call0 <$> ["shotgun", "chaingun"])
    replicateM_ 3 room
    call0 "boring2"
    pure ()

  pure ()
  where
    room =
         traverse call0 ["boring2", "boring2", "rotright"]
      *> call "move" [int 256]

    boring = def "boring" ["x"] $ do
      call "box" (int <$> [0, 128, 160, 256, 256])
      call "pushpop" $ pure $ do
        call "movestep" (coords 96 96)
        call "ibox" (int <$> [24, 128, 200, 64, 64])
        call "movestep" (coords 32 32)
        call0 "x"
        call0 "thing"
        pure ()
      call "move" [int 256]
      pure ()

    boring2 = def "boring2" [] $ do
      call "boring" [choice (call0 <$> ["formerhuman", "formersergeant"])]
      pure ()

newtype IntC a     = IntC Int
newtype StringC a  = StringC String
newtype IncludeC a = IncludeC String
newtype ChoiceC a  = ChoiceC (forall b . [BlockC b])
data IfteC a where
  IfteC :: BlockC c -> BlockC t -> BlockC f -> IfteC a
data DefC a where
  DefC :: String -> [String] -> BlockC b -> DefC a
data CallC a where
  CallC :: String -> [BlockC b] -> CallC a

type ExprF = (IntC :+: StringC :+: CallC :+: ChoiceC :+: IfteC)
newtype ExprC a = ExprC { unExprC :: ExprF a }

data BlockC a where
  BlockC :: (ExprC :<: f, Compilable f) => Ap f b -> BlockC a

-- block :: (ExprC :<: f, Compilable f) => Ap f b -> BlockC a
-- block = BlockC

type ProgF = (ExprC :+: DefC)

type Includes a = Ap IncludeC a
type Program a  = Ap ProgF a
data WadC a = WadC
  { includes :: Includes a
  , program  :: Program a
  }

injectExpr :: (g :<: ExprF, ExprC :<: f) => g a -> Ap f a
injectExpr x = liftAp (inj (ExprC (inj x)))

int :: ExprC :<: f => Int -> Ap f a
int x = injectExpr (IntC x)

coords :: ExprC :<: f => Int -> Int -> [Ap f a]
coords x y = [int x, int y]

string :: ExprC :<: f => String -> Ap f a
string x = injectExpr (StringC x)

include :: IncludeC :<: f => String -> Ap f a
include x = liftAp (inj (IncludeC x))

includeAll :: IncludeC :<: f => [String] -> Ap f ()
includeAll = traverse_ include

choice :: (ExprC :<: f, Compilable f) => [Ap f b] -> Ap f a
choice cs = injectExpr (ChoiceC (fmap BlockC cs))

ifte :: (ExprC :<: g, Compilable g) => Ap g c -> Ap g t -> Ap g f -> Ap g a
ifte c t f = injectExpr (IfteC (BlockC c) (BlockC t) (BlockC f))

def :: (ExprC :=: g, Compilable g) => String -> [String] -> Ap g b -> Ap ProgF a
def n as b = liftAp (inj (DefC n as (BlockC b)))

call :: String -> [Ap ExprC b] -> Ap ExprC a
call n as = injectExpr (CallC n (fmap BlockC as))

call0 :: String -> Ap ExprC a
call0 = (`call` [])

compileC :: WadC a -> String
compileC (WadC is p) = concat
  [ runAp_ ((++ "\n") . compile) is
  , "\n"
  , runAp_ ((++ "\n") . compile) p
  ]

class Compilable f where
  compile :: f a -> String

instance (Compilable f, Compilable g) => Compilable (f :+: g) where
  compile (Inl f) = compile f
  compile (Inr g) = compile g

instance Compilable IntC where
  compile (IntC n) = show n

instance Compilable StringC where
  compile (StringC s) = '"' : s ++ ['"']

instance Compilable IncludeC where
  compile (IncludeC s) = '#' : compile (StringC s)

instance Compilable ChoiceC where
  compile (ChoiceC cs) = let xs = fmap compile cs
                         in  if   length xs > 1
                             then "{ " ++ intercalate " | " xs ++ " }"
                             else concat xs

instance Compilable ExprC where
  compile (ExprC e) = compile e

instance Compilable IfteC where
  compile (IfteC c t f) = concat [compile c, " ? ", compile t, " : ", compile f]

instance Compilable DefC where
  compile (DefC n as (BlockC b)) = concat
    [ n
    , if null as then "" else "(" ++ intercalate ", " as ++ ")"
    , " "
    , let xs = runAp_ (pure . compile) b :: [String]
      in  "{ " ++ intercalate " " xs ++ " }"
    ]

instance Compilable CallC where
  compile (CallC n []) = n
  compile (CallC n as) =
    let blocks = fmap compile as
    in  concat [n, "(", intercalate ", " blocks, ")"]

instance Compilable BlockC where
  compile (BlockC exps) = let xs = runAp_ (pure . compile) exps :: [String]
                          in  if   length xs > 1
                              then "{ " ++ intercalate " " xs ++ " }"
                              else concat xs
