module AoC.Day03Spec where

import SpecHelper

import AoC.Day03

spec :: Spec
spec =
  describe "Day 03" $ do
    it "solves part 1" $ do
      solution <- solvePart01 <$> testInput
      solution `shouldBe` 4361

    it "solves part 2" $ do
      solution <- solvePart02 <$> testInput
      solution `shouldBe` 467835