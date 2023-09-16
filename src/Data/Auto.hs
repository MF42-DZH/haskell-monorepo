{-# LANGUAGE TypeOperators, DeriveFunctor, FlexibleContexts, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ConstraintKinds, UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo, FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs, TupleSections #-}

-- An Antimatter Dimensions Automator DSL for Haskell.
module Data.Auto where

import Control.Monad.Free
import Data.Comp.Ops
import Data.Dynamic
import Data.List
import Data.Proxy

exampleProgram :: AutomatorProgram
exampleProgram = do
  pause (Interval "0.05" S)
  respecAndLoad
  unlock Dilation
  while (Pending RM .<. Numeric "1e2000") $ do
    start Dilation
    respecAndLoad
    pause (Interval "1" S)
    prestige Eternity
    respecAndLoad
    pause (Interval "4" S)
    prestige Eternity
  where
    respecAndLoad = do
      studies Respec
      studies (Load (Name "ALID") False)

exampleProgram2 :: AutomatorProgram
exampleProgram2 = do
  mapM_ (uncurry runChallenge . (, "ALID")) [EC2, EC3, EC5, EC1, EC4, EC8]
  runChallenge EC6 "ALAC"
  runChallenge EC7 "CS07"
  runChallenge EC9 "ALID"
  runChallenge EC10 "CS11"
  runChallenge EC11 "CS11"
  runChallenge EC12 "CS12"
  where
    respecAndLoad tree = do
      studies Respec
      studies (Load (Name tree) False)

    runChallenge ec tree = while (CompletionsOf ec .<. Numeric "5") $ do
      respecAndLoad tree
      start ec
      wait (Pending Completions .>=. Numeric "5")
      prestige Eternity

exampleProgram3 :: AutomatorProgram
exampleProgram3 = do
  while (Numeric "1" `IsLTE` Numeric "2") (notify "ONE")
  while (Numeric "2" `IsLTE` Numeric "3") (notify "TWO")

injectF :: g :<: f => g (Free f a) -> Free f a
injectF = Free . inj

projectF :: g :<: f => Free f a -> Maybe (g (Free f a))
projectF (Free f) = proj f
projectF (Pure _) = Nothing

data AM = AM
  deriving Show
data IP = IP
  deriving Show
data EP = EP
  deriving Show
data RM = RM
  deriving Show
data Infinities = Infinities
  deriving Show
data Eternities = Eternities
  deriving Show
data Realities = Realities
  deriving Show

data GlyphLevel = GlyphLevel
instance Show GlyphLevel where
  show _ = "Glyph Level"

data DT = DT
  deriving Show
data TP = TP
  deriving Show
data RG = RG
  deriving Show
data Rep = Rep
  deriving Show
data TT = TT
  deriving Show
data Completions = Completions
  deriving Show

class Show c => UseableRaw c where
instance UseableRaw AM
instance UseableRaw IP
instance UseableRaw EP
instance UseableRaw RM
instance UseableRaw Infinities
instance UseableRaw Eternities
instance UseableRaw Realities
instance UseableRaw DT
instance UseableRaw TP
instance UseableRaw RG
instance UseableRaw Rep
instance UseableRaw TT

class Show c => UseablePending c
instance UseablePending IP
instance UseablePending EP
instance UseablePending TP
instance UseablePending RM
instance UseablePending GlyphLevel
instance UseablePending Completions

class Show c => UseableTotal c
instance UseableTotal TT
instance UseableTotal Completions

data EC
  =  EC1 |  EC2 |  EC3 |  EC4
  |  EC5 |  EC6 |  EC7 |  EC8
  |  EC9 | EC10 | EC11 | EC12
  deriving (Eq, Ord, Enum, Bounded, Show)

data Currency where
  Raw           :: UseableRaw c => c -> Currency
  Pending       :: UseablePending c => c -> Currency
  Total         :: UseableTotal c => c -> Currency
  Banked        :: Infinities -> Currency
  CompletionsOf :: EC -> Currency
  -- MUST BE IN SCIENFITIC FORM OR DECIMAL FORM!
  Numeric       :: String -> Currency

instance Show Currency where
  show (Raw c)            = show c
  show (Pending c)        = "Pending " ++ show c
  show (Total c)          = "Total " ++ show c
  show (Banked c)         = "Banked " ++ show c
  show (CompletionsOf ec) = show ec ++ " Completions"
  show (Numeric s)        = s

data Comparison
  = IsLT Currency Currency
  | IsLTE Currency Currency
  | IsGTE Currency Currency
  | IsGT Currency Currency

infix 4 .<.
infix 4 .<=.
infix 4 .>=.
infix 4 .>.

(.<.) :: Currency -> Currency -> Comparison
(.<.) = IsLT

(.<=.) :: Currency -> Currency -> Comparison
(.<=.) = IsLTE

(.>=.) :: Currency -> Currency -> Comparison
(.>=.) = IsGTE

(.>.) :: Currency -> Currency -> Comparison
(.>.) = IsGT

instance Show Comparison where
  show (IsLT l r)  = concat [show l, " < ", show r]
  show (IsLTE l r) = concat [show l, " <= ", show r]
  show (IsGTE l r) = concat [show l, " >= ", show r]
  show (IsGT l r)  = concat [show l, " > ", show r]

-- The actual commands begin here!
data Comment t
  = CommentHash String t
  | CommentSlashes String t
  deriving Functor

commentH :: Comment :<: f => String -> Free f ()
commentH c = injectF (CommentHash c (Pure ()))

commentS :: Comment :<: f => String -> Free f ()
commentS c = injectF (CommentSlashes c (Pure ()))

data Notify t
  = Notify String t
  deriving Functor

notify :: Notify :<: f => String -> Free f ()
notify msg = injectF (Notify msg (Pure ()))

data TimeUnit
  = MS
  | S | Sec | Seconds
  | M | Min | Minutes
  | H | Hours
  deriving (Show, Eq, Enum, Bounded)

data Interval
  = Interval String TimeUnit -- See comment for Numeric constructor for Currency.

instance Show Interval where
  show (Interval s tu) = unwords [s, show tu]

class Show a => Pauseable a
instance Pauseable Interval
instance Pauseable String

data Pause t where
  Pause :: Pauseable p => p -> t -> Pause t

instance Functor Pause where
  fmap f (Pause p t) = Pause p (f t)

pause :: (Pauseable p, Pause :<: f) => p -> Free f ()
pause intv = injectF (Pause intv (Pure ()))

data While t
  = While Comparison Dynamic t
  deriving Functor

while :: (Typeable f, While :<: f) => Comparison -> Free f () -> Free f ()
while cond body = injectF (While cond (toDyn body) (Pure ()))

data If t
  = If Comparison Dynamic t
  deriving Functor

if' :: (Typeable f, If :<: f) => Comparison -> Free f () -> Free f ()
if' cond body = injectF (If cond (toDyn body) (Pure ()))

data Toggle
  = On | Off
  deriving (Show, Eq, Ord, Enum, Bounded)

data BlackHole t
  = SetBlackHole Toggle t
  deriving Functor

setBlackHole :: BlackHole :<: f => Toggle -> Free f ()
setBlackHole state = injectF (SetBlackHole state (Pure ()))

data StoredTime t
  = SetStoreState Toggle t
  | UseStoredTime t
  deriving Functor

setTimeStorage :: StoredTime :<: f => Toggle -> Free f ()
setTimeStorage state = injectF (SetStoreState state (Pure ()))

useStoredTime :: StoredTime :<: f => Free f ()
useStoredTime = injectF (UseStoredTime (Pure ()))

class Nowait f where
  setNowait :: f -> f

data Dilation = Dilation
  deriving Show

class Show f => Feature f
instance Feature EC
instance Feature Dilation

data Unlock t where
  Unlock :: Feature f => f -> Bool -> t -> Unlock t

instance Functor Unlock where
  fmap f (Unlock ft nw t) = Unlock ft nw (f t)

instance Nowait (Unlock f) where
  setNowait (Unlock f _ t) = Unlock f True t

setNowait' :: forall f a . Nowait (f (Free f a)) => Free f a -> Free f a
setNowait' t = case projectF t of
  Just (x :: f (Free f a)) -> injectF (setNowait x)
  Nothing                  -> t

unlock :: (Feature ft, Unlock :<: f) => ft -> Free f ()
unlock ft = injectF (Unlock ft False (Pure ()))

class Respec f where
  setRespec :: f -> f

setRespec' :: forall f a . Respec (f (Free f a)) => Free f a -> Free f a
setRespec' t = case projectF t of
  Just (x :: f (Free f a)) -> injectF (setRespec x)
  Nothing                  -> t

data Infinity = Infinity
  deriving Show
data Eternity = Eternity
  deriving Show
data Reality  = Reality
  deriving Show

class Show c => CanPrestigeAt c
instance CanPrestigeAt Infinity
instance CanPrestigeAt Eternity
instance CanPrestigeAt Reality

class Show c => CanWaitFor c
instance {-# OVERLAPPING #-} CanWaitFor Comparison
instance {-# OVERLAPPABLE #-} (Show c, CanPrestigeAt c) => CanWaitFor c

data Wait t where
  Wait :: CanWaitFor cond => cond -> t -> Wait t

instance Functor Wait where
  fmap f (Wait c t) = Wait c (f t)

wait :: (CanWaitFor cond, Wait :<: f) => cond -> Free f ()
wait c = injectF (Wait c (Pure ()))

data Until t where
  Until :: CanWaitFor c => c -> Dynamic -> t -> Until t

instance Functor Until where
  fmap f (Until c b t)         = Until c b (f t)

until :: (CanWaitFor c, Typeable f, Until :<: f) => c -> Free f () -> Free f ()
until cond body = injectF (Until cond (toDyn body) (Pure ()))

data Prestige t where
  Prestige :: CanPrestigeAt layer => layer -> Bool -> Bool -> t -> Prestige t

instance Functor Prestige where
  fmap f (Prestige l nw r t) = Prestige l nw r (f t)

instance Nowait (Prestige t) where
  setNowait (Prestige l _ r t) = Prestige l True r t

instance Respec (Prestige t) where
  -- WARNING: You cannot respec Infinities.
  setRespec (Prestige l nw _ t) = Prestige l nw True t

prestige :: (CanPrestigeAt layer, Prestige :<: f) => layer -> Free f ()
prestige layer = injectF (Prestige layer False False (Pure ()))

data Start t where
  Start :: Feature f => f -> t -> Start t

instance Functor Start where
  fmap f (Start ft t) = Start ft (f t)

start :: (Feature ft, Start :<: f) => ft -> Free f ()
start ft = injectF (Start ft (Pure ()))

-- IDs are restricted to 1-6, but this is checked in AD, not here.
newtype Id   = Id Int
newtype Name = Name String

instance Show Id where
  show (Id i) = "ID " ++ show i

instance Show Name where
  show (Name n) = "NAME " ++ n

class Show l => Loadable l
instance Loadable Id
instance Loadable Name

data Range = Int :-> Int

instance Show Range where
  show (f :-> t) = concat [show f, "-", show t]

newtype StudyList = StudyList [Int]

instance Show StudyList where
  show (StudyList xs) = intercalate "," (fmap show xs)

data StudyPath
  = AntimatterPath | InfinityPath | TimePath
  | ActivePath     | PassivePath  | IdlePath
  | LightPath      | DarkPath

instance Show StudyPath where
  show AntimatterPath = "Antimatter"
  show InfinityPath   = "Infinity"
  show TimePath       = "Time"
  show ActivePath     = "Active"
  show PassivePath    = "Passive"
  show IdlePath       = "Idle"
  show LightPath      = "Light"
  show DarkPath       = "Dark"

class Show p => Purchasable p
instance Purchasable Range
instance Purchasable StudyList
instance Purchasable StudyPath

data StudyCommand where
  Respec   :: StudyCommand
  Purchase :: Purchasable p => p -> Bool -> StudyCommand
  Load     :: Loadable l => l -> Bool -> StudyCommand

instance Show StudyCommand where
  show Respec = "RESPEC"
  show (Purchase p nw)
    = let nwt = if nw then "NOWAIT " else ""
      in  nwt ++ "PURCHASE " ++ show p
  show (Load l nw)
    = let nwt = if nw then "NOWAIT " else ""
      in  nwt ++ "LOAD " ++ show l

data Studies t
  = Studies StudyCommand t
  deriving Functor

instance Nowait (Studies t) where
  setNowait (Studies (Purchase p _) t) = Studies (Purchase p True) t
  setNowait (Studies (Load l _) t)     = Studies (Load l True) t
  setNowait s                      = s

-- Destructive implementation.
instance Respec (Studies t) where
  setRespec (Studies _ t) = Studies Respec t

studies :: Studies :<: f => StudyCommand -> Free f ()
studies c = injectF (Studies c (Pure ()))

class (Show p, Show c) => HasCurrency p c where
  getCurrency :: Proxy p -> c

instance HasCurrency Infinity IP where
  getCurrency _ = IP

instance HasCurrency Eternity EP where
  getCurrency _ = EP

instance HasCurrency Reality RM where
  getCurrency _ = RM

-- See comment for Numeric constructor for Currency.
newtype Multiplier = Multiplier String

instance Show Multiplier where
  show (Multiplier m) = m

-- See comment for Numeric constructor for Currency.
newtype Amount p c = Amount String

instance HasCurrency p c => Show (Amount p c) where
  show (Amount amt) = unwords [amt, show (getCurrency (Proxy :: Proxy p) :: c)]

class Show l => PreReality l
instance PreReality Infinity
instance PreReality Eternity

class (Show p, Show s) => AutobuyerSetting p s
instance CanPrestigeAt p => AutobuyerSetting p Toggle
instance PreReality p => AutobuyerSetting p Interval
instance PreReality p => AutobuyerSetting p Multiplier
instance (CanPrestigeAt p, HasCurrency p c) => AutobuyerSetting p (Amount p c)

data Auto t where
  Auto :: AutobuyerSetting p s => p -> s -> t -> Auto t

instance Functor Auto where
  fmap f (Auto p s t) = Auto p s (f t)

auto :: (AutobuyerSetting p s, Auto :<: f) => p -> s -> Free f ()
auto p s = injectF (Auto p s (Pure ()))

type Automator =
  (   Studies
  :+: Prestige :+: Unlock    :+: Start
  :+: Auto     :+: BlackHole :+: StoredTime
  :+: Notify   :+: Comment
  :+: Wait     :+: Pause     :+: If         :+: Until :+: While
  )

type AutomatorProgram = Free Automator ()

class Typeable f => Compile f where
  compile :: (Typeable a, Compile g) => f (Free g a) -> [String]

compile' :: (Typeable a, Compile f) => Free f a -> [String]
compile' (Free f) = compile f
compile' (Pure _) = []

pretty :: AutomatorProgram -> String
pretty = intercalate "\n" . compile'

instance (Compile f, Compile g) => Compile (f :+: g) where
  compile (Inl l) = compile l
  compile (Inr r) = compile r

instance Compile Comment where
  compile (CommentHash c next)    = ("# " ++ c) : compile' next
  compile (CommentSlashes c next) = ("// " ++ c) : compile' next

instance Compile Notify where
  compile (Notify msg next) = ("NOTIFY \"" ++ msg ++ "\"") : compile' next

instance Compile Pause where
  compile (Pause interval next) = ("PAUSE " ++ show interval) : compile' next

instance Compile Wait where
  compile (Wait cond next) = ("WAIT " ++ show cond) : compile' next

instance Compile If where
  compile :: forall g a . (Typeable a, Compile g) => If (Free g a) -> [String]
  compile (If cond body next) =
    (("IF " ++ show cond ++ " {") : fmap ("\t" ++) (compile' (fromDyn body undefined :: Free g a)) ++ ["}"]) ++ compile' next

instance Compile While where
  compile :: forall g a . (Typeable a, Compile g) => While (Free g a) -> [String]
  compile (While cond body next) =
    (("WHILE " ++ show cond ++ " {") : fmap ("\t" ++) (compile' (fromDyn body undefined :: Free g a)) ++ ["}"]) ++ compile' next

instance Compile Until where
  compile :: forall g a . (Typeable a, Compile g) => Until (Free g a) -> [String]
  compile (Until cond body next) =
    (("UNTIL " ++ show cond ++ " {") : fmap ("\t" ++) (compile' (fromDyn body undefined :: Free g a)) ++ ["}"]) ++ compile' next

instance Compile Prestige where
  compile (Prestige l nw r next) =
    let nwt = if nw then " NOWAIT" else ""
        rst = if r  then " RESPEC" else ""
    in  (show l ++ nwt ++ rst) : compile' next

instance Compile Unlock where
  compile (Unlock ft nw next) =
    let nwt = if nw then " NOWAIT" else ""
    in  ("UNLOCK " ++ nwt ++ show ft) : compile' next

instance Compile Start where
  compile (Start ft next) = ("START " ++ show ft) : compile' next

instance Compile Studies where
  compile (Studies cmd next) = ("STUDIES " ++ show cmd) : compile' next

instance Compile BlackHole where
  compile (SetBlackHole st next) = ("BLACK HOLE " ++ show st) : compile' next

instance Compile StoredTime where
  compile (SetStoreState st next) = ("STORE GAME TIME " ++ show st) : compile' next
  compile (UseStoredTime next)    = "STORE GAME TIME Use" : compile' next

instance Compile Auto where
  compile (Auto p set next) = unwords ["AUTO", show p, show set] : compile' next
