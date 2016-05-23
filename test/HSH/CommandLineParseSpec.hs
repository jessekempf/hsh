module HSH.CommandLineParseSpec (spec) where

import Test.Hspec

import HSH.CommandLineParse

spec :: Spec
spec =
  describe "the parser" $ do
    it "parses basic builtin and non-builtin commands" $ do
      parseLine "setenv foo bar" `shouldBe` SetEnv "foo" "bar"
      parseLine "getenv foo" `shouldBe` GetEnv "foo"
      parseLine "some command" `shouldBe` ExternalCommand "some" ["command"]
      parseLine "ls -l" `shouldBe` ExternalCommand "ls" ["-l"]

    -- XXX: This violates the principle of least astonishment but at least it is a
    -- XXX: _defined_ behavior.
    it "treats incorrectly called builtins as external commands" $
      parseLine "setenv foo" `shouldBe` ExternalCommand "setenv" ["foo"]
