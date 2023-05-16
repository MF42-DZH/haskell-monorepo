{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes, ScopedTypeVariables #-}

module Data.ALaCarte where

import Control.Monad.Free
import Data.Comp.Ops
import Data.Proxy

data Expr f = In (f (Expr f))

data Val e = Val Int
  deriving Functor
type IntExpr = Expr Val

data Add e = Add e e
  deriving Functor
type AddExpr = Expr Add

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

injectE :: (g :<: f) => g (Expr f) -> Expr f
injectE = In . inj

infixl 6 +++
(+++) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x +++ y = injectE (Add x y)

val :: (Val :<: f) => Int -> Expr f
val x = injectE (Val x)

addExample2 :: Expr (Val :+: Add)
addExample2 = val 30000 +++ val 1330 +++ val 7

-- Memory example.
data Store t     = Store Int t
  deriving Functor
newtype Recall t = Recall (Int -> t)
  deriving Functor
newtype Clear t  = Clear t
  deriving Functor

injectF :: g :<: f => g (Free f a) -> Free f a
injectF = Free . inj

store :: Store :<: f => Int -> Free f ()
store x = injectF (Store x (Pure ()))

recall :: Recall :<: f => Free f Int
recall = injectF (Recall Pure)

clear :: Clear :<: f => Free f ()
clear = injectF (Clear (Pure ()))

tick :: (Functor f, Store :<: f, Recall :<: f) => Free f Int
tick = recall >>= \ y -> y <$ store (y + 1)

execute :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
execute fin _ (Pure x)    = fin x
execute fin comp (Free t) = comp (fmap (execute fin comp) t)

newtype Mem = Mem Int
  deriving Show

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run Store where
  runAlgebra (Store x r) _ = r (Mem x)

instance Run Recall where
  runAlgebra (Recall f) (Mem i) = f i (Mem i)

instance Run Clear where
  runAlgebra (Clear r) _ = r (Mem 0)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r

run :: Run f => Free f a -> Mem -> (a, Mem)
run = execute (,) runAlgebra

example :: (Int, Mem)
example = run (tick :: Free (Store :+: Recall) Int) (Mem 4)

-- RPN Calculator
data Push a t    = Push a t
  deriving Functor
newtype Pop a t  = Pop (Proxy a, t)
  deriving Functor
newtype Dup a t  = Dup (Proxy a, t)
  deriving Functor
newtype Swap a t = Swap (Proxy a, t)
  deriving Functor
newtype Rot a t  = Rot (Proxy a, t)
  deriving Functor
data Op1 a t     = Op1 (a -> a) t
  deriving Functor
data Op2 a t     = Op2 (a -> a -> a) t
  deriving Functor

push :: Push a :<: f => a -> Free f ()
push x = injectF (Push x (Pure ()))

pop ::  Pop a :<: f => Proxy a -> Free f ()
pop prox = injectF (Pop (prox, Pure ()))

dup :: Dup a :<: f => Proxy a -> Free f ()
dup prox = injectF (Dup (prox, Pure ()))

swap :: Swap a :<: f => Proxy a -> Free f ()
swap prox = injectF (Swap (prox, Pure ()))

rot :: Rot a :<: f => Proxy a -> Free f ()
rot prox = injectF (Rot (prox, Pure ()))

op1 :: Op1 a :<: f => (a -> a) -> Free f ()
op1 f = injectF (Op1 f (Pure ()))

op2 :: Op2 a :<: f => (a -> a -> a) -> Free f ()
op2 f = injectF (Op2 f (Pure ()))

class Functor f => RPN f a where
  runRPN :: f ([a] -> [a]) -> ([a] -> [a])

instance (RPN f a, RPN g a) => RPN (f :+: g) a where
  runRPN (Inl r) = runRPN r
  runRPN (Inr r) = runRPN r

instance RPN (Push a) a where
  runRPN (Push x r) xs = r (x : xs)

instance RPN (Pop a) a where
  runRPN (Pop (_, r)) ~(_ : xs) = r xs

instance RPN (Dup a) a where
  runRPN (Dup (_, r)) ~(x : xs) = r (x : x : xs)

instance RPN (Swap a) a where
  runRPN (Swap (_, r)) ~(x : y : xs) = r (y : x : xs)

instance RPN (Rot a) a where
  runRPN (Rot (_, r)) ~(x : y : z : xs) = r (z : x : y : xs)

instance RPN (Op1 a) a where
  runRPN (Op1 f r) ~(x : xs) = r (f x : xs)

instance RPN (Op2 a) a where
  runRPN (Op2 f r) ~(y : x : xs) = r (f x y : xs)

type FullRPN a = Push a :+: Pop a :+: Rot a :+: Dup a :+: Op1 a :+: Op2 a :+: Swap a

quadForm :: forall a . Floating a => Free (FullRPN a) ()
quadForm = do
  let rot'  = rot (Proxy :: Proxy a)
      dup'  = dup (Proxy :: Proxy a)
      swap' = swap (Proxy :: Proxy a)
      neg   = op1 (negate :: a -> a)
      sqrt' = op1 (sqrt :: a -> a)
      add   = op2 ((+) :: a -> a -> a)
      sub   = op2 ((-) :: a -> a -> a)
      mul   = op2 ((*) :: a -> a -> a)
      div'  = op2 ((/) :: a -> a -> a)
  rot' -- b c a
  dup'
  add -- b c 2a
  dup'
  dup'
  add -- b c 2a 4a
  rot' -- b 2a 4a c
  mul -- b 2a 4ac
  rot' -- 2a 4ac b
  dup'
  dup'
  mul -- 2a 4ac b b^2
  rot' -- 2a b b^2 4ac
  sub -- 2a b (b^2-4ac)
  sqrt'
  swap'
  neg -- 2a sqrt(b^2-4ac) (-b)
  dup'
  rot'
  dup' -- 2a (-b) (-b) sqrt(b^2-4ac) sqrt(b^2-4ac)
  rot' -- 2a (-b) sqrt(b^2-4ac) sqrt(b^2-4ac) (-b)
  add -- 2a (-b) sqrt(b^2-4ac) (sqrt(b^2-4ac)+(-b))
  swap'
  rot'
  swap'
  sub -- 2a (sqrt(b^2-4ac)+(-b)) ((-b)-sqrt(b^2-4ac))
  rot'
  dup' -- (sqrt(b^2-4ac)+(-b)) ((-b)-sqrt(b^2-4ac)) 2a 2a
  rot'
  swap'
  div' -- (sqrt(b^2-4ac)+(-b)) 2a RNEG
  rot'
  rot'
  div'
  swap' -- FINISH

evalRPN :: (Num a, RPN f a) => Free f () -> [a] -> [a]
evalRPN = execute seq runRPN

evalQF :: Double -> Double -> Double -> [Double]
evalQF a b c = evalRPN (quadForm :: Free (FullRPN Double) ()) [c, b, a]
