module Main where

import qualified Text.Printf as Printf

import qualified AoC.Day01 as Day01
import qualified AoC.Day02 as Day02
import qualified AoC.Day03 as Day03
import qualified AoC.Day04 as Day04

type Solver = ([String] -> Int)

run :: IO [String] -> Solver -> IO Int
run input solver = do
  return =<< solver <$> input

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
  putStrLn =<< runAll Day02.dataInput Day02.solvePart01 Day01.solvePart02
  
  putStrLn "Day 03"
  putStrLn =<< runAll Day03.dataInput Day03.solvePart01 Day01.solvePart02
  
  putStrLn "Day 04"
  putStrLn =<< runAll Day04.dataInput Day04.solvePart01 Day01.solvePart02