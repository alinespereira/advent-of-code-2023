module Main where

import AoC.Day01

main :: IO ()
main = do
  testData <- testInput
  input <- dataInput
  let testResult01 = solvePart01 testData
  let testResult02 = solvePart02 testData
  let result01 = solvePart01 input
  let result02 = solvePart02 input
  putStrLn $ "[TEST] Part 01: " ++ show testResult01
  putStrLn $ "[TEST] Part 02: " ++ show testResult02
  putStrLn $ "[DATA] Part 01: " ++ show result01
  putStrLn $ "[DATA] Part 02: " ++ show result02