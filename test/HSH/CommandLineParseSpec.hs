module HSH.CommandLineParseSpec (spec) where

import Test.Hspec

import HSH.CommandLineParse

spec :: Spec
spec =
  describe "the parser" $
    it "parses basic builtin and non-builtin commands" $ do
      parseLine "setenv foo bar" `shouldBe` SetEnv "foo" "bar"
      parseLine "getenv foo" `shouldBe` GetEnv "foo"
      parseLine "some command" `shouldBe` ExternalCommand "some" ["command"]
      parseLine "ls -l" `shouldBe` ExternalCommand "ls" ["-l"]
