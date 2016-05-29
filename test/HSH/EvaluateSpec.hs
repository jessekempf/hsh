module HSH.EvaluateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.State
import qualified Data.Map as Map

import HSH.CommandLineParse
import HSH.Evaluate
import HSH.ShellState

spec :: Spec
spec =
  describe "evaluate" $
    describe "setenv" $
      it "sets environment variables" $
        execStateT (evaluate $ SetEnv "foo" "bar") defaultShellState
        `shouldReturn`
        setEnv "foo" "bar" defaultShellState
