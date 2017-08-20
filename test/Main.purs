module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Prelude (discard, ($))
import Test.Unit (failure, success, suite, test, testSkip)
import Test.Unit.Karma (runKarma)

main :: forall eff. Eff ( avar :: AVAR, console :: CONSOLE | eff) Unit
main = runKarma do
  suite "runKarma test suite" $ do
    test "run this test" $ success
    testSkip "skip this test" $ failure "ups"
