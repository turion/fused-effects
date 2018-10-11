{-# LANGUAGE DeriveFunctor, ExistentialQuantification, PolyKinds, StandaloneDeriving #-}
module Control.Effect.Fresh where

import Control.Effect.Handler

data Fresh m k
  = Fresh (Int -> k)
  | forall b . Reset Int (m b) (b -> k)

deriving instance Functor (Fresh m)

instance HFunctor Fresh where
  hfmap _ (Fresh     k) = Fresh k
  hfmap f (Reset i m k) = Reset i (f m) k
