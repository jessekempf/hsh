module HSH.EvaluateSpec (spec) where

import Test.Hspec

import Control.Monad.State
import qualified Data.Map as Map

import HSH.CommandLineParse
import HSH.Evaluate

spec :: Spec
spec =
  describe "executeCommand" $
    describe "setenv" $
      it "sets environment variables" $
        execStateT (executeCommand $ SetEnv "foo" "bar") defaultShellState
        `shouldReturn`
        ShellState { envVars = Map.singleton "foo" "bar" }
