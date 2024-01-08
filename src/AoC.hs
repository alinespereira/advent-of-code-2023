module AoC
  ( module Day01,
    module Day02,
    module Day03,
    module Day04,
  )
where

import qualified AoC.Day01 as Day01
import qualified AoC.Day02 as Day02
import qualified AoC.Day03 as Day03
import qualified AoC.Day04 as Day04

-- type Solver = [String] -> Int

-- run :: [String] -> [String] -> Solver -> Solver -> IO ()
-- run test input solvePart01 solvePart02 = do
--   let testResult01 = solvePart01 test
--   let testResult02 = solvePart02 test
--   let result01 = solvePart01 input
--   let result02 = solvePart02 input
--   putStrLn $ "[TEST] Part 01: " ++ show testResult01
--   putStrLn $ "[TEST] Part 02: " ++ show testResult02
--   putStrLn $ "[DATA] Part 01: " ++ show result01
--   putStrLn $ "[DATA] Part 02: " ++ show result02