module Main
( main
) where

import qualified Accum
import qualified Catch
import qualified Choose
import qualified Cull
import qualified Cut
import qualified Cut.Church
import qualified Empty
import qualified Error
import qualified Fail
import qualified Fresh
import qualified Fusion
import qualified Lift
import qualified NonDet
import qualified NonDet.Church
import qualified Reader
import qualified State
import           Test.Tasty
import qualified Throw
import qualified Writer

main :: IO ()
main = defaultMain $ testGroup "unit tests"
  [ Accum.tests
  , Catch.tests
  , Choose.tests
  , Cull.tests
  , Cut.tests
  , Cut.Church.tests
  , Empty.tests
  , Error.tests
  , Fail.tests
  , Fresh.tests
  , Fusion.tests
  , Lift.tests
  , NonDet.tests
  , NonDet.Church.tests
  , Reader.tests
  , State.tests
  , Throw.tests
  , Writer.tests
  ]
