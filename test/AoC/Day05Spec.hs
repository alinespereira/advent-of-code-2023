module AoC.Day05Spec where

import AoC.Day05
import Data.Range
import SpecHelper
import Text.Parsec (parse)

spec :: Spec
spec =
  describe "Day 05" $ do
    context "input parser" $ do
      it "parses a list of seeds" $ do
        let s = "seeds: 1 2 3"
        parse seeds "" s `shouldBe` Right [1, 2, 3]

      it "parses a map label" $ do
        let s = "seed-to-soil map:"
        parse describerFromTo "" s `shouldBe` Right ("seed", "soil")

      it "parses a map range" $ do
        let s = "50 98 2"
        let r = Describer (50 +=* 52) (98 +=* 100)
        parse describerRanges "" s `shouldBe` Right r

    it "solves part 1" $ do
      solution <- solvePart01 <$> testInput
      solution `shouldBe` 35

    it "solves part 2" $ do
      solution <- solvePart02 <$> testInput
      solution `shouldBe` 46