module Test.Unit.Karma
  ( runKarma
  ) where

import Prelude

import Control.Monad.Free (resume)
import Control.Monad.State (State, execState, modify)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Test.Unit (Group(..), TestF(..), TestSuite, walkSuite)
import Test.Unit.Main (run, runTestWith)

type Run
   = ({total :: Int} -> Effect Unit)
  -> ({id :: String, description :: String, suite :: Array String, log :: Array String, success :: Boolean, skipped :: Boolean} -> Effect Unit)
  -> Effect Unit
  -> Effect Unit

foreign import _runKarma :: Run -> Effect Unit

countTests :: TestSuite -> State Int Unit
countTests tst =
  case resume tst of
       Left (TestGroup (Group _ tst') _ _ tst'') ->
         do
           countTests tst'
           countTests tst''
       Left (TestUnit _ _ _ _ tst') ->
         do
           _ <- modify (add 1)
           countTests tst'
       Left (SkipUnit _ tst') ->
         do
           _ <- modify (add 1)
           countTests tst'
       Right _ -> pure unit

-- | Run a test suite using karma test runner.
runKarma
  :: TestSuite
  -> Effect Unit
runKarma = _runKarma <<< createRunner
  where
  createRunner
    :: TestSuite
    -> Run
  createRunner suite = \info result complete ->
    let
      total = execState (countTests suite) 0
      karmaRunner tst = walkSuite runSuiteItem tst
        where
          runSuiteItem path (Left _) = pure unit
          runSuiteItem path (Right (Tuple label t)) = do
            res <- attempt t
            case res of
              (Right _) -> do
                liftEffect $ result {id: label, suite: (foldl A.snoc [] path), description: label, log: [], success: true, skipped: false}
              (Left err) -> do
                liftEffect $ result {id: label, suite: (foldl A.snoc [] path), description: label, log: [message err], success: false, skipped: false}
            pure unit
      in do
        info { total }
        run $ runTestWith karmaRunner suite
        complete
