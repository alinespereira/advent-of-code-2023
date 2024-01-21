module AoC.Day08 where

import AoC (readLines)
import qualified Data.Map as M

testInput :: IO ([String], [String])
testInput = do
  test01 <- readLines "data/Day08/test01.txt"
  test02 <- readLines "data/Day08/test02.txt"
  return (test01, test02)

dataInput :: IO [String]
dataInput = readLines "data/Day08/data.txt"

data Mode
  = ModeLeft
  | ModeRight
  deriving (Show, Eq)

instance Read Mode where
  readsPrec _ "L" = [(ModeLeft, "")]
  readsPrec _ "R" = [(ModeRight, "")]
  readsPrec _ _ = []

type Network = M.Map String (String, String)

parseInput :: [String] -> ([Mode], Network)
parseInput input = (modes, network)
  where
    modes = parseModes (head input)
    network = parseNetwork (drop 2 input)

parseModes :: String -> [Mode]
parseModes = map modeFromChar
  where
    modeFromChar 'L' = ModeLeft
    modeFromChar 'R' = ModeRight
    modeFromChar _ = error "Invalid mode"

parseNetwork :: [String] -> Network
parseNetwork = M.fromList . map parseNode

parseNode :: String -> (String, (String, String))
parseNode = asEntry . words . filter (`elem` validKeyChars)
  where
    uppercase = ['A' .. 'Z']
    space = [' ']
    digit = ['0' .. '9']
    validKeyChars = uppercase ++ space ++ digit
    asEntry :: [String] -> (String, (String, String))
    asEntry [node, left, right] = (node, (left, right))
    asEntry _ = error "Invalid node"

walkUntil :: String -> 
             (String -> Bool) ->
             [Mode] -> 
             Network -> 
             [(String, Int)]
walkUntil _ _  [] _ = error "No modes provided"
walkUntil from stop modes network = 
  go from stop (cycle modes') network
  where
    modes' = zip modes [0..]
    go _ _ [] _ = error "No path"
    go f s ((m, i):ms) net
      | s f = [(f, i)]
      | otherwise = 
        let f' = step f m net
          in (f, i) : go f' s ms net

walk :: String -> String -> [Mode] -> Network -> [(String, Int)]
walk from to = walkUntil from (==to)

ghostWalk :: [String] -> [String] -> [Mode] -> Network -> [([String], Mode)]
ghostWalk froms tos modes network = 
  (froms, head modes) : ghostGo froms tos (cycle modes) network
  where
    ghostGo _ _ [] _ = error "No path"
    ghostGo fs ts (m:ms) net
      | all (`elem` ts) fs = []
      | otherwise =
        let fs' = map (\f -> step f m net) fs
          in (fs', m) : ghostGo fs' ts ms net

step :: String -> Mode -> Network -> String
step node mode net =
  let (left, right) = M.findWithDefault ("", "") node net
  in case mode of
    ModeLeft -> left
    ModeRight -> right

solvePart01 :: [String] -> Int
solvePart01 = (`subtract` 1) . length . uncurry (walk "AAA" "ZZZ") . parseInput

solvePart02 :: [String] -> Int
solvePart02 input =
  let (modes, net) = parseInput input
      fs = filter (\k -> last k == 'A') $ M.keys net
      ts = filter (\k -> last k == 'Z') $ M.keys net
    in (`subtract` 1) $ length $ ghostWalk fs ts modes net