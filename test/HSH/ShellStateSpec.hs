module HSH.ShellStateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.State
import qualified Data.Map as Map

import HSH.MonitoredDirectory
import HSH.ShellState

spec :: Spec
spec = do
  describe "shellPrompt" $ do
    it "has a sensible default in case the PROMPT env var is unset." $
      shellPrompt defaultShellState { envVars = Map.empty } `shouldBe` "Prompt Undefined > "
    it "bases the shell prompt on what is in the environment" $ property $
      \promptstr -> shellPrompt (setEnv "PROMPT" promptstr defaultShellState) == promptstr ++ " "

  describe "resolveExecutable" $ do
    let testShellState = defaultShellState {
      pathDirs = [
        MonitoredDirectory "/sbin" 0 $ Map.singleton "s.bad" $ QualifiedFilePath "/sbin/s.bad",
        MonitoredDirectory "/bin" 0 $ Map.singleton "thecheat" $ QualifiedFilePath "/bin/thecheat"
      ]
    }

    it "returns the original command if the lookup table is empty" $
      resolveExecutable defaultShellState "foobar" `shouldBe` "foobar"

    it "returns the original command if no command is found in the lookup table" $
      resolveExecutable testShellState "foobar" `shouldBe` "foobar"

    it "returns the fully-qualified path for a command if it is found in the lookup table" $
      resolveExecutable testShellState "thecheat" `shouldBe` "/bin/thecheat"
