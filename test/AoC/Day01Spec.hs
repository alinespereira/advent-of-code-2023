module AoC.Day01Spec where

import SpecHelper

import AoC.Day01

spec :: Spec
spec =
  describe "Day 01" $ do
    it "solves part 1" $ do
      solutionPart01 <- solvePart01 . fst <$> testInput
      solutionPart01 `shouldBe` 142

    it "solves part 2" $ do
      solutionPart02 <- solvePart02 . snd <$> testInput
      solutionPart02 `shouldBe` 281