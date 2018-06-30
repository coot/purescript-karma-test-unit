module Test.Main where

import Effect (Effect)
import Data.Unit (Unit)
import Prelude (discard, ($))
import Test.Unit (failure, success, suite, test, testSkip)
import Test.Unit.Karma (runKarma)

main :: Effect Unit
main = runKarma do
  suite "runKarma test suite" $ do
    test "run this test" $ success
    testSkip "skip this test" $ failure "this test should have been skipped"
