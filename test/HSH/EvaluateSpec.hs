module HSH.EvaluateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.State
import qualified Data.Map as Map

import HSH.CommandLineParse
import HSH.Evaluate

spec :: Spec
spec = do
  describe "evaluate" $
    describe "setenv" $
      it "sets environment variables" $
        execStateT (evaluate $ SetEnv "foo" "bar") defaultShellState
        `shouldReturn`
        setEnv "foo" "bar" defaultShellState

  describe "shellPrompt" $ do
    it "has a sensible default in case the PROMPT env var is unset." $
      shellPrompt defaultShellState { envVars = Map.empty } `shouldBe` "Prompt Undefined > "
    it "bases the shell prompt on what is in the environment" $ property $
      \promptstr -> shellPrompt (setEnv "PROMPT" promptstr defaultShellState) == promptstr ++ " "
