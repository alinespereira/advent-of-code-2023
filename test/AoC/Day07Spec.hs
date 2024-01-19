module AoC.Day07Spec where

import SpecHelper

import AoC.Day07

spec :: Spec
spec =
  describe "Day 07" $ do
    it "solves part 1" $ do
      solution <- solvePart01 <$> testInput
      solution `shouldBe` 6440

    it "solves part 2" $ do
      solution <- solvePart02 <$> testInput
      solution `shouldBe` 5905
