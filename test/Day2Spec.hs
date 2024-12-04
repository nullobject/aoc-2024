module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec = do
  describe "pairs" $ do
    it "groups pairs of numbers" $ do
      let ns = [1, 2, 3, 4, 5]
      pairs ns `shouldBe` [(1, 2), (2, 3), (3, 4), (4, 5)]

  describe "parseReports" $ do
    it "parses a list of reports" $ do
      let s =
            "7 6 4 2 1\n\
            \1 2 7 8 9\n\
            \9 7 6 2 1\n\
            \1 3 2 4 5\n\
            \8 6 4 4 1\n\
            \1 3 6 7 9"
      parseReports s
        `shouldBe` [ [7, 6, 4, 2, 1]
                   , [1, 2, 7, 8, 9]
                   , [9, 7, 6, 2, 1]
                   , [1, 3, 2, 4, 5]
                   , [8, 6, 4, 4, 1]
                   , [1, 3, 6, 7, 9]
                   ]

  describe "checkReport" $ do
    it "checks a report" $ do
      let ns = [7, 6, 4, 2, 1]
      checkReport ns `shouldBe` True

  describe "part1" $ do
    it "calculates the result" $ do
      let s =
            "7 6 4 2 1\n\
            \1 2 7 8 9\n\
            \9 7 6 2 1\n\
            \1 3 2 4 5\n\
            \8 6 4 4 1\n\
            \1 3 6 7 9"
      part1 s `shouldBe` 2
