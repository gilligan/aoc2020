{-# LANGUAGE OverloadedStrings #-}

module AOC.Day4Spec (spec) where

import AOC.Day04
import Test.Hspec

spec :: Spec
spec = do
  describe "AOC" $ do
    describe "parseEntry" $ do
      it "parse inputs according to spec" $ do
        parseEntry "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm" `shouldBe` Just [("byr", "1937"), ("iyr", "2017"), ("eyr", "2020"), ("hgt", "183cm"), ("hcl", "#fffffd"), ("ecl", "gry"), ("pid", "860033327"), ("cid", "147")]
        parseEntry "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929" `shouldBe` Nothing
        parseEntry "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm" `shouldBe` Just [("byr", "1931"), ("iyr", "2013"), ("eyr", "2024"), ("hgt", "179cm"), ("hcl", "#ae17e1"), ("ecl", "brn"), ("pid", "760753108")]
        parseEntry "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in" `shouldBe` Nothing

--it "accepts a valid entry" $ do
--parseEntry "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm" `shouldBe` Just Entry
