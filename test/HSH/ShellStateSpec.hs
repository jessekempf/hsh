module HSH.ShellStateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import HSH.MonitoredDirectory
import HSH.ShellState

genChar :: Gen Char
genChar = elements ['\0' .. '\xff']

genUndefinedEnvVar :: ShellState -> Gen String
genUndefinedEnvVar ShellState{envVars = env} =
  listOf genChar `suchThat` (\str -> not (str `Set.member` definedEnvVars))
  where
    definedEnvVars = Set.fromList $ Map.keys env

spec :: Spec
spec = do
  describe "setEnv" $
    it "sets an environment variable" $ property $
      \name val -> setEnv name val defaultShellState{envVars = Map.empty} == defaultShellState{envVars = Map.singleton name val}

  describe "getEnv" $ do
    it "retrieves environment variables set by setEnv" $ property $
      \name val -> getEnv name (setEnv name val defaultShellState) == Just val
    it "returns Nothing if an environment variable is unset" $ forAll (genUndefinedEnvVar defaultShellState) $
      \name -> isNothing $ getEnv name defaultShellState

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
