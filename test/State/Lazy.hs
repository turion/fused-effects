{-# LANGUAGE TypeApplications #-}
module State.Lazy
( tests
) where

import Control.Carrier.State.Lazy
import Hedgehog.Function
import Pure
import State.Gen
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "State.Lazy.StateC"
  [ testProperty "get state" . forall (genA :. fn @A (Blind <$> genM genState genA) :. Nil) $
    \ a k -> get_state (~=) runState a (getBlind . apply k)
  , testProperty "put update" . forall (genA :. genA :. fmap Blind (genM genState genA) :. Nil) $
    \ a b m -> put_update (~=) runState a b (getBlind m)
  ]
