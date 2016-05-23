module HSH.DataStructures.PrefixTreeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified HSH.DataStructures.PrefixTree as PT

spec :: Spec
spec =
  describe "PrefixTree" $
    describe "lookup" $ do
      let exampleTree =
                        PT.insert "abcd" "foo" $
                        PT.insert "abe" "simpson" $
                        PT.insert "red" "ridinghood"
                        PT.empty

      it "does stuff" $
        show exampleTree `shouldBe` "foo"

      it "returns Nothing when given an empty tree" $
        PT.lookup "abcd" (PT.empty :: PT.PrefixTree String) `shouldBe` Nothing
      it "returns Nothing when a key is not found" $
        PT.lookup "24601" exampleTree `shouldBe` Nothing
      it "returns Maybe v when the key is found" $ do
        PT.lookup "abcd" exampleTree `shouldBe` Just "foo"
        PT.lookup "abe" exampleTree `shouldBe` Just "simpson"
        PT.lookup "red" exampleTree `shouldBe` Just "ridinghood"
