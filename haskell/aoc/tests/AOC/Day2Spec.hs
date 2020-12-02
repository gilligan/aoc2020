{-# LANGUAGE OverloadedStrings #-}

module AOC.Day2Spec (spec) where

import AOC.Day02
import Data.Text
import Test.Hspec

puzzleInput :: [Int]
puzzleInput =
  []

spec :: Spec
spec = do
  describe "AOC" $ do
    describe "part1" $ do
      describe "parsePwdRule" $ do
        it "parses correctly" $ do
          parsePwdRule "1-3 a" `shouldBe` PwdRule 1 3 'a'
          parsePwdRule "2-2 b" `shouldBe` PwdRule 2 2 'b'
      describe "parseEntry" $ do
        it "parses correctly" $ do
          parseEntry "1-3 a: abcde" `shouldBe` Entry (PwdRule 1 3 'a') "abcde"
      describe "isValidEntry" $ do
        it "tests entries correctly" $ do
          isValidEntry (mkEntry 1 3 'a' "abcd") `shouldBe` True
          isValidEntry (mkEntry 1 3 'a' "bcd") `shouldBe` False
      describe "isValidEntry'" $ do
        it "tests entries correctly" $ do
          isValidEntry' (parseEntry "2-9 c: ccccccccc") `shouldBe` False
          isValidEntry' (parseEntry "1-3 a: abcde") `shouldBe` True
