{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Main where

import Control.Concurrent.Fake
import Control.Monad.Trans.State.Strict
import Data.Comp.Ops
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import Data.Sequence ( Seq )
import qualified Data.Sequence as S

-- Instructions.
newtype Snd id e = Snd (Either Int Char)
  deriving Functor
newtype Rcv id e = Rcv Char
  deriving Functor
data Set id e = Set Char (Either Int Char)
  deriving Functor
data Add id e = Add Char (Either Int Char)
  deriving Functor
data Mul id e = Mul Char (Either Int Char)
  deriving Functor
data Mod id e = Mod Char (Either Int Char)
  deriving Functor
data Jgz id e = Jgz (Either Int Char) (Either Int Char)
  deriving Functor

type BaseInst id = Snd id :+: Rcv id :+: Set id :+: Add id :+: Mul id :+: Mod id :+: Jgz id

sndI :: forall id f a . Snd id :<: f => Proxy id -> Int -> f a
sndR :: forall id f a . Snd id :<: f => Proxy id -> Char -> f a
sndI _ = (inj :: Snd id a -> f a) . Snd . Left
sndR _ = (inj :: Snd id a -> f a) . Snd . Right

rcv :: forall id f a . Rcv id :<: f => Proxy id -> Char -> f a
rcv _ = (inj :: Rcv id a -> f a) . Rcv

setI :: forall id f a . Set id :<: f => Proxy id -> Char -> Int -> f a
setR :: forall id f a . Set id :<: f => Proxy id -> Char -> Char -> f a
setI _ r = (inj :: Set id a -> f a) . Set r . Left
setR _ r = (inj :: Set id a -> f a) . Set r . Right

addI :: forall id f a . Add id :<: f => Proxy id -> Char -> Int -> f a
addR :: forall id f a . Add id :<: f => Proxy id -> Char -> Char -> f a
addI _ r = (inj :: Add id a -> f a) . Add r . Left
addR _ r = (inj :: Add id a -> f a) . Add r . Right

mulI :: forall id f a . Mul id :<: f => Proxy id -> Char -> Int -> f a
mulR :: forall id f a . Mul id :<: f => Proxy id -> Char -> Char -> f a
mulI _ r = (inj :: Mul id a -> f a) . Mul r . Left
mulR _ r = (inj :: Mul id a -> f a) . Mul r . Right

modI :: forall id f a . Mod id :<: f => Proxy id -> Char -> Int -> f a
modR :: forall id f a . Mod id :<: f => Proxy id -> Char -> Char -> f a
modI _ r = (inj :: Mod id a -> f a) . Mod r . Left
modR _ r = (inj :: Mod id a -> f a) . Mod r . Right

jgzII :: forall id f a . Jgz id :<: f => Proxy id -> Int -> Int -> f a
jgzIR :: forall id f a . Jgz id :<: f => Proxy id -> Int -> Char -> f a
jgzRI :: forall id f a . Jgz id :<: f => Proxy id -> Char -> Int -> f a
jgzRR :: forall id f a . Jgz id :<: f => Proxy id -> Char -> Char -> f a
jgzII _ cond off = (inj :: Jgz id a -> f a) (Jgz (Left cond) (Left off))
jgzIR _ cond off = (inj :: Jgz id a -> f a) (Jgz (Left cond) (Right off))
jgzRI _ cond off = (inj :: Jgz id a -> f a) (Jgz (Right cond) (Left off))
jgzRR _ cond off = (inj :: Jgz id a -> f a) (Jgz (Right cond) (Right off))

class HasSoundState a where
  pushSound :: Int -> a -> a
  getSound  :: a -> Int

class HasRegisterState id a where
  modifyRegister :: Proxy id -> Char -> (Int -> Int) -> a -> a
  getRegister    :: Proxy id -> Char -> a -> Int

value :: HasRegisterState () s => Either Int Char -> State s Int
value (Left x)  = return x
value (Right s) = getRegister pu s <$> get

class HasProgramCounter id a where
  jump  :: Proxy id -> Int -> a -> a
  getPC :: Proxy id -> a -> Int

type P1State = (Int, Int, Map Char Int)

instance HasSoundState P1State where
  pushSound x (pc, _, r) = (pc, x, r)
  getSound (_, s, _)     = s

instance HasRegisterState () P1State where
  modifyRegister _ k f (pc, s, m) = (pc, s, M.alter (maybe (Just (f 0)) (Just . f)) k m)
  getRegister    _ k (_, _, m)    = fromMaybe 0 (m M.!? k)

instance HasProgramCounter () P1State where
  jump _ x (pc, s, m) = (pc + x, s, m)
  getPC _ (pc, _, _)  = pc

class Functor f => P1 f where
  singleP1 :: (HasProgramCounter () s, HasSoundState s, HasRegisterState () s) => f a -> State s (Maybe Int)

evalP1 :: P1 f => Seq (f e) -> Int
evalP1 instrs = evalState comp ((0, 0, M.empty) :: P1State)
  where
    comp = do
      pc <- getPC pu <$> get
      case instrs S.!? pc of
        Nothing -> error "The program shouldn't have done that."
        Just x  -> singleP1 x >>= (\ case
          Nothing -> comp
          Just y  -> return y)

instance (P1 f, P1 g) => P1 (f :+: g) where
  singleP1 (Inl f) = singleP1 f
  singleP1 (Inr g) = singleP1 g

instance P1 (Snd id) where
  singleP1 (Snd x) = Nothing <$ do
    content <- value x
    modify' (jump pu 1 . pushSound content)

pu :: Proxy ()
pu = Proxy

instance P1 (Rcv id) where
  singleP1 (Rcv _) = Just . getSound <$> get

instance P1 (Set id) where
  singleP1 (Set d x) = Nothing <$ do
    content <- value x
    modify' (jump pu 1 . modifyRegister pu d (const content))

instance P1 (Add id) where
  singleP1 (Add d x) = Nothing <$ do
    content <- value x
    modify' (jump pu 1 . modifyRegister pu d (+ content))

instance P1 (Mul id) where
  singleP1 (Mul d x) = Nothing <$ do
    content <- value x
    modify' (jump pu 1 . modifyRegister pu d (* content))

instance P1 (Mod id) where
  singleP1 (Mod d x) = Nothing <$ do
    content <- value x
    modify' (jump pu 1 . modifyRegister pu d (`rem` content))

instance P1 (Jgz id) where
  singleP1 (Jgz cond off) = Nothing <$ do
    c' <- value cond
    x' <- value off
    if   c' > 0
    then modify' (jump pu x')
    else modify' (jump pu 1)

testProgram :: forall id e . Seq (BaseInst id e)
testProgram = let prx = Proxy :: Proxy id in
  S.fromList
    [ setI prx 'a' 1
    , addI prx 'a' 2
    , mulR prx 'a' 'a'
    , modI prx 'a' 5
    , sndR prx 'a'
    , setI prx 'a' 5
    , rcv prx 'a'
    , jgzRI prx 'a' (-1)
    , setI prx 'a' 1
    , jgzRI prx 'a' (-2)
    ]

-- Part 2 datatypes.
data One
data Two

class HasEnqueue id a where
  enq' :: Proxy id -> Int -> a -> a

class HasDequeue id a where
  deq' :: Proxy id -> a -> (Either Int Int, a)

class HasSends id a where
  incrS :: Proxy id -> a -> a
  sends :: Proxy id -> a -> Int

data P2State = P2State
  { p2t1pc   :: Int
  , p2t2pc   :: Int
  , p2t1q    :: Seq Int
  , p2t2q    :: Seq Int
  , p2t1ret  :: Int
  , p2t2ret  :: Int
  , p2t1regs :: Map Char Int
  , p2t2regs :: Map Char Int
  , p1Sends  :: Int
  }

mkP2State :: P2State
mkP2State = P2State 0 0 S.empty S.empty 0 0 (M.singleton 'p' 0) (M.singleton 'p' 1) 0

instance HasProgramCounter One P2State where
  jump _ off x = x { p2t1pc = p2t1pc x + off }
  getPC _      = p2t1pc

instance HasProgramCounter Two P2State where
  jump _ off x = x { p2t2pc = p2t2pc x + off }
  getPC _      = p2t2pc

instance HasRegisterState One P2State where
  modifyRegister _ k f s = s { p2t1regs = M.alter (maybe (Just (f 0)) (Just . f)) k (p2t1regs s) }
  getRegister _ c s      = fromMaybe 0 (p2t1regs s M.!? c)

instance HasRegisterState Two P2State where
  modifyRegister _ k f s = s { p2t2regs = M.alter (maybe (Just (f 0)) (Just . f)) k (p2t2regs s) }
  getRegister _ c s      = fromMaybe 0 (p2t2regs s M.!? c)

instance HasSends One P2State where
  incrS _ s = s { p1Sends = p1Sends s + 1 }
  sends _   = p1Sends

p1 :: Proxy One
p1 = Proxy

p2 :: Proxy Two
p2 = Proxy

v1 :: HasRegisterState One s => Either Int Char -> State s Int
v2 :: HasRegisterState Two s => Either Int Char -> State s Int
v1 (Left x)  = return x
v1 (Right s) = getRegister p1 s <$> get
v2 (Left x)  = return x
v2 (Right s) = getRegister p2 s <$> get

instance HasEnqueue One P2State where
  enq' _ x s = s { p2t1q = p2t1q s S.|> x }

instance HasDequeue One P2State where
  deq' _ s   = case p2t1q s of
    S.Empty      -> let s' = s { p2t1ret = p2t1ret s + 1 } in (Left (p2t1ret s'), s')
    (x S.:<| xs) -> (Right x, s { p2t1ret = 0, p2t1q = xs })

instance HasEnqueue Two P2State where
  enq' _ x s = s { p2t2q = p2t2q s S.|> x }

instance HasDequeue Two P2State where
  deq' _ s   = case p2t2q s of
    S.Empty      -> let s' = s { p2t2ret = p2t2ret s + 1 } in (Left (p2t2ret s'), s')
    (x S.:<| xs) -> (Right x, s { p2t2ret = 0, p2t2q = xs })

class Functor f => P2 s f where
  singleP2 :: f a -> ThreadT (State s) (Maybe Int)

instance (P2 s f, P2 s g) => P2 s (f :+: g) where
  singleP2 (Inl l) = singleP2 l
  singleP2 (Inr r) = singleP2 r

instance (HasProgramCounter One s, HasRegisterState One s, HasEnqueue Two s, HasSends One s) => P2 s (Snd One) where
  singleP2 (Snd x) = Nothing <$ atomic (do
    content <- v1 x
    modify' (jump p1 1 . incrS p1 . enq' p2 content))

instance (HasProgramCounter Two s, HasRegisterState Two s, HasEnqueue One s) => P2 s (Snd Two) where
  singleP2 (Snd x) = Just 0 <$ atomic (do
    content <- v2 x
    modify' (jump p2 1 . enq' p1 content))

instance (HasProgramCounter One s, HasRegisterState One s, HasDequeue One s, HasSends One s) => P2 s (Rcv One) where
  singleP2 (Rcv r) = do
    (attempt, s') <- atomic (deq' p1 <$> get)
    atomic (put s')
    
    case attempt of
      Left x  -> if x >= 4 then atomic (Just . sends p1 <$> get) >>= Result else atomic (return Nothing)
      Right x -> Nothing <$ atomic (modify' (jump p1 1 . modifyRegister p1 r (const x)))

instance (HasProgramCounter Two s, HasRegisterState Two s, HasDequeue Two s) => P2 s (Rcv Two) where
  singleP2 (Rcv r) = do
    (attempt, s') <- atomic (deq' p2 <$> get)
    atomic (put s')
    
    case attempt of
      Left x  -> if x >= 4 then Result (Just 0) else atomic (return (Just 0))
      Right x -> Just 0 <$ atomic (modify' (jump p2 1 . modifyRegister p2 r (const x)))

instance (HasProgramCounter One s, HasRegisterState One s) => P2 s (Set One) where
  singleP2 (Set r x) = Nothing <$ atomic (do
    content <- v1 x
    modify' (jump p1 1 . modifyRegister p1 r (const content)))

-- TODO: More instances...

testProgramTempl :: Proxy id -> Seq (BaseInst id e)
testProgramTempl prx = S.fromList
  [ sndI prx 1
  , sndI prx 2
  , sndR prx 'p'
  , rcv prx 'a'
  , rcv prx 'b'
  , rcv prx 'c'
  , rcv prx 'd'
  ]

progT1 :: forall e . Seq (BaseInst One e)
progT1 = testProgramTempl (Proxy :: Proxy One)

progT2 :: forall e . Seq (BaseInst Two e)
progT2 = testProgramTempl (Proxy :: Proxy Two)

main :: IO ()
main = print (evalP1 testProgram)
