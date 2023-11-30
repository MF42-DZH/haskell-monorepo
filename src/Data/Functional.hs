-- {-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Functional where

-- class Term t where

-- data Var where
--   Var :: String -> Var
--   deriving Eq

-- data App where
--   App :: Replace t => Fun -> t -> App

-- data Fun where
--   Fun :: Replace t => Var -> t -> Fun

-- instance Term Var
-- instance Term App
-- instance Term Fun

-- instance (Term t, Term t') => Term (Either t t')

-- type TDSL = Either Var (Either App Fun)

-- class Term t => Inj t where
--   inj :: t -> TDSL

-- instance Inj Var where
--   inj = Left

-- instance Inj App where
--   inj = Right . Left

-- instance Inj Fun where
--   inj = Right . Right

-- instance Inj TDSL where
--   inj = id

-- var :: String -> TDSL
-- var = inj . Var

-- app :: Replace t => Fun -> t -> TDSL
-- app = (inj .) . App

-- fun :: Replace t => Var -> t -> TDSL
-- fun = (inj .) . Fun

-- class Inj t => Replace t where
--   replace :: t -> Var -> TDSL -> TDSL

-- instance Replace Var where
--   replace x y body
--     | x == y    = body
--     | otherwise = inj x

-- instance Replace App where
--   replace (App (Fun arg b) x) v body = inj (App (Fun arg (replace b v body)) (replace x v body))

-- instance Replace Fun where
--   replace org@(Fun arg b) v body
--     | arg == v  = inj org
--     | otherwise = inj (Fun arg (replace b v body))

-- instance (Inj t, Inj t', Replace t, Replace t') => Replace (Either t t') where
--   replace (Left x) v b  = replace x v b
--   replace (Right x) v b = replace x v b

-- class Term t => Reduce t where
--   reduce :: t -> TDSL

-- instance Reduce Var where
--   reduce = inj

-- instance Reduce App where
--   reduce (App (Fun arg body) inp)
--     = replace body arg (inj inp)

-- instance Reduce Fun where
--   reduce = inj

-- instance (Reduce t, Reduce t') => Reduce (Either t t') where
--   reduce = either reduce reduce
