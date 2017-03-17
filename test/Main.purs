module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Prelude (($))
import Test.Unit (success, suite, test)
import Test.Unit.Karma (runKarma)

main :: forall eff. Eff ( avar :: AVAR, console :: CONSOLE | eff) Unit
main = runKarma do
  suite "hello" $
    test "world!" $ success
