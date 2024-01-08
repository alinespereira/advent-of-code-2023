module AoC.Day01Spec where

import SpecHelper

import AoC.Day01

spec :: Spec
spec =
  describe "Day 01" $ do
    it "solves part 1" $ do
      solution <- solvePart01 <$> testInput
      solution `shouldBe` 142

    it "solves part 2" $ do
      solution <- solvePart02 <$> testInput
      solution `shouldBe` 281