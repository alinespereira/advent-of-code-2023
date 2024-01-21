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
parseNode = asEntry . words . filter (`elem` uppercase ++ space)
  where
    uppercase = ['A' .. 'Z']
    space = [' ']
    asEntry :: [String] -> (String, (String, String))
    asEntry [node, left, right] = (node, (left, right))
    asEntry _ = error "Invalid node"

walkPath :: String -> String -> [Mode] -> Network -> [(String, String)]
walkPath from to modes = go from to (cycle modes)
  where
    go _ _ [] _ = error "No path"
    go f t (m:ms) net
      | f == t = [(f, t)]
      | otherwise = 
        let f' = step f m net
          in (f, f') : go f' t ms net

ghostWalk :: [String] -> [String] -> [Mode] -> Network -> [([String], [String])]
ghostWalk froms tos modes = ghostGo froms tos (cycle modes)
  where
    ghostGo _ _ [] _ = error "No path"
    ghostGo fs ts (m:ms) net
      | and (zipWith (==) fs ts) = [(fs, ts)]
      | otherwise =
        let fs' = map (\f -> step f m net) fs
          in (fs, fs') : ghostGo fs' ts ms net

step :: String -> Mode -> Network -> String
step node mode net =
  let (left, right) = M.findWithDefault ("", "") node net
  in case mode of
    ModeLeft -> left
    ModeRight -> right

solvePart01 :: [String] -> Int
solvePart01 = 
  length . uncurry (walkPath "AAA" "ZZZ") . parseInput

solvePart02 :: [String] -> Int
solvePart02 input =
  let (modes, net) = parseInput input
      fs = filter (\k -> last k == 'A') $ M.keys net
      ts = filter (\k -> last k == 'Z') $ M.keys net
    in length $ ghostWalk fs ts modes net