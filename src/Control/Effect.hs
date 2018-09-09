{-# LANGUAGE EmptyCase, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternSynonyms, PolyKinds, RankNTypes, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Control.Effect where

import Control.Applicative (Alternative(..))
import Control.Monad (ap, liftM)
import Control.Monad.Fail
import Control.Monad.IO.Class

data Eff effects a
  = Return a
  | Eff (effects (Eff effects) a)


class Effect sig where
  emap :: Monad m => (m a -> m b) -> (sig m a -> sig m b)

  handle :: (Monad m, Monad n, Functor c) => c () -> (forall x . c (m x) -> n (c x)) -> (sig m a -> sig n (c a))

inject :: Subset effect sig => effect (Eff sig) a -> Eff sig a
inject = Eff . inj

project :: Subset effect sig => Eff sig a -> Maybe (effect (Eff sig) a)
project (Eff op) = prj op
project _        = Nothing


data Void m a

run :: Eff Void a -> a
run (Return a) = a
run (Eff v) = case v of {}


newtype Lift sig m a = Lift { unLift :: sig (m a) }

instance Functor sig => Effect (Lift sig) where
  emap f (Lift op) = Lift (fmap f op)
  handle state handler (Lift op) = Lift (fmap (\ p -> handler (p <$ state)) op)

instance Subset (Lift IO) sig => MonadIO (Eff sig) where
  liftIO = inject . Lift . fmap pure

runM :: Monad m => Eff (Lift m) a -> m a
runM (Return a) = return a
runM (Eff (Lift op)) = op >>= runM


data (f :+: g) (m :: * -> *) a
  = L (f m a)
  | R (g m a)
  deriving (Eq, Ord, Show)

instance (Effect l, Effect r) => Effect (l :+: r) where
  emap f (L l) = L (emap f l)
  emap f (R r) = R (emap f r)

  handle state handler (L l) = L (handle state handler l)
  handle state handler (R r) = R (handle state handler r)


pattern Other :: r (Eff (l :+: r)) a -> Eff (l :+: r) a
pattern Other s = Eff (R s)


data NonDet m a
  = Empty'
  | Choose' (Bool -> m a)

pattern Empty :: Subset NonDet effects => Eff effects a
pattern Empty <- (project -> Just Empty')

pattern Choose :: Subset NonDet effects => (Bool -> Eff effects a) -> Eff effects a
pattern Choose k <- (project -> Just (Choose' k))

instance Subset NonDet sig => Alternative (Eff sig) where
  empty = inject Empty'
  l <|> r = inject (Choose' (\ c -> if c then l else r))


data Reader r m a
  = Ask' (r -> m a)
  | forall b . Local' (r -> r) (m b) (b -> m a)

pattern Ask :: Subset (Reader r) effects => (r -> Eff effects a) -> Eff effects r
pattern Ask k <- (project -> Just (Ask' k))

pattern Local :: Subset (Reader r) effects => (r -> r) -> Eff effects b -> (b -> Eff effects a) -> Eff effects a
pattern Local f m k <- (project -> Just (Local' f m k))

ask :: Subset (Reader r) sig => Eff sig r
ask = inject (Ask' pure)

local :: Subset (Reader r) sig => (r -> r) -> Eff sig a -> Eff sig a
local f m = inject (Local' f m pure)


data State s m a
  = Get' (s -> m a)
  | Put' s (m a)

pattern Get :: Subset (State s) effects => (s -> Eff effects a) -> Eff effects s
pattern Get k <- (project -> Just (Get' k))

pattern Put :: Subset (State s) effects => s -> Eff effects a -> Eff effects ()
pattern Put s k <- (project -> Just (Put' s k))

get :: Subset (State s) sig => Eff sig s
get = inject (Get' pure)

put :: Subset (State s) sig => s -> Eff sig ()
put s = inject (Put' s (pure ()))


data Fail m a = Fail' String

pattern Fail :: Subset Fail effects => String -> Eff effects a
pattern Fail s <- (project -> Just (Fail' s))

instance Subset Fail sig => MonadFail (Eff sig) where
  fail = inject . Fail'


class (Effect sub, Effect sup) => Subset sub sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance Effect sub => Subset sub sub where
  inj = id
  prj = Just

instance (Effect sub, Effect sup) => Subset sub (sub :+: sup) where
  inj = L . inj
  prj (L f) = Just f
  prj _     = Nothing

instance (Effect sub', Subset sub sup) => Subset sub (sub' :+: sup) where
  inj = R . inj
  prj (R g) = prj g
  prj _     = Nothing


instance Effect sig => Functor (Eff sig) where
  fmap = liftM

instance Effect sig => Applicative (Eff sig) where
  pure = Return
  (<*>) = ap

instance Effect sig => Monad (Eff sig) where
  Return v >>= k = k v
  Eff op >>= k = Eff (emap (>>= k) op)
