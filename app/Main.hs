module Main where

import qualified AoC.Day01 as Day01
import qualified AoC.Day02 as Day02
import qualified AoC.Day03 as Day03
import qualified AoC.Day04 as Day04
import qualified AoC.Day05 as Day05
import qualified AoC.Day06 as Day06
import qualified AoC.Day07 as Day07
import qualified AoC.Day08 as Day08
import qualified AoC.Day09 as Day09
import qualified Text.Printf as Printf

type Solver = ([String] -> Int)

run :: IO [String] -> Solver -> IO Int
run input solver = do
  solver <$> input

runAll :: IO [String] -> Solver -> Solver -> IO String
runAll input solverPart01 solverPart02 = do
  result01 <- run input solverPart01
  result02 <- run input solverPart02
  return $
    Printf.printf "\tPart 01: %d\n\tPart 02: %d" result01 result02

main :: IO ()
main = do
  putStrLn "Day 01"
  putStrLn =<< runAll Day01.dataInput Day01.solvePart01 Day01.solvePart02

  putStrLn "Day 02"
  putStrLn =<< runAll Day02.dataInput Day02.solvePart01 Day02.solvePart02

  putStrLn "Day 03"
  putStrLn =<< runAll Day03.dataInput Day03.solvePart01 Day03.solvePart02

  putStrLn "Day 04"
  putStrLn =<< runAll Day04.dataInput Day04.solvePart01 Day04.solvePart02

  -- putStrLn "Day 05"
  -- putStrLn =<< runAll Day05.dataInput Day05.solvePart01 Day05.solvePart02

  putStrLn "Day 06"
  putStrLn =<< runAll Day06.dataInput Day06.solvePart01 Day06.solvePart02

  putStrLn "Day 07"
  putStrLn =<< runAll Day07.dataInput Day07.solvePart01 Day07.solvePart02

  putStrLn "Day 08"
  putStrLn =<< runAll Day08.dataInput Day08.solvePart01 Day08.solvePart02

  putStrLn "Day 09"
  putStrLn =<< runAll Day09.dataInput Day09.solvePart01 Day09.solvePart02
