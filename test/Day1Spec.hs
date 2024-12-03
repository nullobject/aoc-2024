module Day1Spec where

import Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "joinDigits" $ do
    it "joins two digits" $ do
      joinDigits ("1", "2") `shouldBe` Just 12
      joinDigits ("one", "two") `shouldBe` Just 12
      joinDigits ("foo", "bar") `shouldBe` Nothing

  describe "parseDigit" $ do
    it "parses a digit to an integer" $ do
      parseDigit "1" `shouldBe` Just 1
      parseDigit "one" `shouldBe` Just 1
      parseDigit "foo" `shouldBe` Nothing

  describe "findDigits" $ do
    it "finds numeric digits" $ do
      let ps = ["1", "2", "3"]
      findDigits ps "1foo2bar3" `shouldBe` Just ("1", "3")
      findDigits ps "foo1bar" `shouldBe` Just ("1", "1")
      findDigits ps "foo" `shouldBe` Nothing
      findDigits ps "" `shouldBe` Nothing

    it "finds word digits" $ do
      let ps = ["foo", "bar", "baz"]
      findDigits ps "1foo2bar3" `shouldBe` Just ("foo", "bar")
      findDigits ps "foo1bar" `shouldBe` Just ("foo", "bar")
      findDigits ps "foo" `shouldBe` Just ("foo", "foo")
      findDigits ps "" `shouldBe` Nothing

  describe "part1" $ do
    it "calculates the result" $ do
      let s =
            "1abc2\n\
            \pqr3stu8vwx\n\
            \a1b2c3d4e5f\n\
            \treb7uchet"
      part1 s `shouldBe` Just 142

  describe "part2" $ do
    it "calculates the result" $ do
      let s =
            "two1nine\n\
            \eightwothree\n\
            \abcone2threexyz\n\
            \xtwone3four\n\
            \4nineeightseven2\n\
            \zoneight234\n\
            \7pqrstsixteen"
      part2 s `shouldBe` Just 281
