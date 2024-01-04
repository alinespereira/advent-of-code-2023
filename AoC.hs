module AoC where

type Solver = [String] -> Int

run :: [String] -> [String] -> Solver -> Solver -> IO ()
run test input solvePart01 solvePart02 = do
  let testResult01 = solvePart01 test
  let testResult02 = solvePart02 test
  let result01 = solvePart01 input
  let result02 = solvePart02 input
  putStrLn $ "[TEST] Part 01: " ++ show testResult01
  putStrLn $ "[TEST] Part 02: " ++ show testResult02
  putStrLn $ "[DATA] Part 01: " ++ show result01
  putStrLn $ "[DATA] Part 02: " ++ show result02