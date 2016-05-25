module HSH.MonitoredDirectorySpec (spec) where

import Data.List
import qualified Data.Map as Map

import System.FilePath

import HSH.MonitoredDirectory
import Test.Hspec

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

genChar :: Gen Char
genChar = elements ['\0' .. '\xff']

filenameGen :: Gen FilePath
filenameGen =  listOf genChar `suchThat` (\str -> not ("/" `isInfixOf` str))

dirGen :: Gen String
dirGen = listOf genChar

pathGen :: Gen (String, [FilePath])
pathGen = dirGen >*< listOf filenameGen

spec :: Spec
spec = do
  describe "isVisible" $ do
    it "is true for /foo/bar" $ isVisible (QualifiedFilePath "/foo/bar") `shouldBe` True
    it "is false for /foo/.bar" $ isVisible (QualifiedFilePath "/foo/.bar") `shouldBe` False
    it "is false for /foo/..bar" $ isVisible (QualifiedFilePath "/foo/..bar") `shouldBe` False
    it "is true for /foo" $ isVisible (QualifiedFilePath "/foo") `shouldBe` True
    it "is false for /.foo" $ isVisible (QualifiedFilePath "/.foo") `shouldBe` False

  describe "onlyVisible" $
    it "returns only visible filenames" $
      onlyVisible (map QualifiedFilePath ["/foo/bar", "/.bar"]) `shouldBe` [QualifiedFilePath "/foo/bar"]

  describe "commandTableFrom" $
    it "takes qualified paths and produces a table mapping filenames to full paths" $ forAll pathGen $
      \(dir, names) ->
        let path name = (dir ++ "/" ++ name) in
        let paths = map path names in
          commandTableFrom (map (QualifiedFilePath . path) names)
          ==
          Map.fromList (map (\name -> (name, QualifiedFilePath (path name))) names)
