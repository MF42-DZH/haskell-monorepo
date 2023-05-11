{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleContexts #-}

module Data.ALaCarte where

import Data.Comp.Ops

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
