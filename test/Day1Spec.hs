module Day1Spec where

import Data.Map (fromList)
import Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "parseNumbers" $ do
    it "parses two numbers" $ do
      parseNumbers "1 2" `shouldBe` [1, 2]
      parseNumbers "1  2" `shouldBe` [1, 2]
      parseNumbers "1   2" `shouldBe` [1, 2]

  describe "parseLists" $ do
    it "parses two numbers" $ do
      parseLists "1 2\n3 4" `shouldBe` [[1, 3], [2, 4]]

  describe "histogram" $ do
    it "calculates frequencies of elements" $ do
      histogram [1, 1, 2, 2, 3, 1] `shouldBe` fromList [(1, 3), (2, 2), (3, 1)]

  describe "part1" $ do
    it "calculates the result" $ do
      let s =
            "3 4\n\
            \4 3\n\
            \2 5\n\
            \1 3\n\
            \3 9\n\
            \3 3"
      part1 s `shouldBe` 11

  describe "part2" $ do
    it "calculates the result" $ do
      let s =
            "3 4\n\
            \4 3\n\
            \2 5\n\
            \1 3\n\
            \3 9\n\
            \3 3"
      part2 s `shouldBe` 31
