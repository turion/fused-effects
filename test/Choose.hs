{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
module Choose
( tests
, genChoose
, chooseTests
) where

import qualified Control.Carrier.Choose.Church as ChooseC
import Control.Effect.Choose
import Data.List.NonEmpty
import Hedgehog
import Hedgehog.Function
import Hedgehog.Gen
import Pure
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Choose"
  [ testGroup "ChooseC"  $ chooseTests (ChooseC.runChooseS (pure . pure))
  , testGroup "NonEmpty" $ chooseTests (pure . toList)
  ] where
  chooseTests :: Has Choose sig m => (forall a . m a -> PureC [a]) -> [TestTree]
  chooseTests run = Choose.chooseTests run (genM [genChoose]) genA genB


genChoose :: Has Choose sig m => Gen a -> Gen (m a) -> Gen (m a)
genChoose _ m = subterm2 m m (<|>)


chooseTests :: forall a b m sig . (Has Choose sig m, Arg a, Eq a, Eq b, Show a, Show b, Vary a) => (forall a . m a -> PureC [a]) -> (forall a . Gen a -> Gen (Blind (m a))) -> Gen a -> Gen b -> [TestTree]
chooseTests runChoose m a b =
  [ testProperty "<|> distributivity" . forall (m a :. m a :. fn @a (m b) :. Nil) $
    \ m n k -> choose_distributivity (~=) runChoose (getBlind m) (getBlind n) (getBlind . apply k)
  , testProperty "<|> associativity" . forall (m a :. m a :. m a :. Nil) $
    \ m n o -> choose_associativity (~=) runChoose (getBlind m) (getBlind n) (getBlind o)
  ]
