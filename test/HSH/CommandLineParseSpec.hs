module HSH.CommandLineParseSpec (spec) where

import Test.Hspec

import HSH.CommandLineParse
import HSH.ShellState

spec :: Spec
spec = do
  let testEnv = setEnv "FOO" "BAR" defaultShellState

  describe "token expander" $ do
    it "maps FOO to FOO" $
      tokenExpand "FOO" defaultShellState `shouldBe` Just "FOO"
    it "maps ${FOO} to BAR" $
      tokenExpand "${FOO}" testEnv `shouldBe` Just "BAR"
    it "returns Nothing for an attempt to expand an undefined variable" $
      tokenExpand "${UNDEFINED}" defaultShellState `shouldBe` Nothing

  describe "line expander" $ do
    it "returns nothing if the line is syntacitcally invalid" $
      expand (words "echo ${FOO}") defaultShellState `shouldBe` Nothing
    it "does environment variable substitution where possible" $
      expand (words "echo ${FOO}") testEnv `shouldBe` Just (words "echo BAR")
    
  describe "the parser" $ do
    let parseLine_ line = parseLine line defaultShellState

    it "parses basic builtin and non-builtin commands" $ do
      parseLine_ "setenv foo bar" `shouldBe` Just (SetEnv "foo" "bar")
      parseLine_ "getenv foo" `shouldBe` Just (GetEnv "foo")
      parseLine_ "some command" `shouldBe` Just (ExternalCommand "some" ["command"])
      parseLine_ "showstate" `shouldBe` Just DebugState
      parseLine_ "cd newdir" `shouldBe` Just (Chdir "newdir")
      parseLine_ "ls -l" `shouldBe` Just (ExternalCommand "ls" ["-l"])

    -- XXX: This violates the principle of least astonishment but at least it is a
    -- XXX: _defined_ behavior.
    it "treats incorrectly called builtins as external commands" $
      parseLine "setenv foo" defaultShellState `shouldBe` Just (ExternalCommand "setenv" ["foo"])

