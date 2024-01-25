module AoC.Day08 where

import AoC (readLines)
-- import Control.Applicative (liftA2)
-- import Data.List (findIndices, inits)
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

walkUntil ::
  String ->
  (String -> Bool) ->
  [Mode] ->
  Network ->
  [(String, Int)]
walkUntil _ _ [] _ = error "No modes provided"
walkUntil from stop modes network =
  go from stop (cycle modes') network
  where
    modes' = zip modes [0 ..]
    go _ _ [] _ = error "No path"
    go f s ((m, i) : ms) net
      | s f = [(f, i)]
      | otherwise =
          let f' = step f m net
           in (f, i) : go f' s ms net

walk :: String -> String -> [Mode] -> Network -> [(String, Int)]
walk from to = walkUntil from (== to)

-- infiniteWalk :: String -> [Mode] -> Network -> [(String, Int)]
-- infiniteWalk from = walkUntil from (const False)

-- cycleWalk ::
--   String ->
--   [Mode] ->
--   Network ->
--   ([(String, Int)], [(String, Int)])
-- cycleWalk from modes network = break (== cycleStartsAt) path
--   where
--     path =
--       last . takeWhile f . drop 2 . inits $
--         infiniteWalk from modes network
--     f = liftA2 notElem last init
--     (x, m) = last path
--     cycleStartsAt = (step x (modes !! m) network, m + 1)

ghostWalk :: [String] -> (String -> Bool) -> [Mode] -> Network -> Int
ghostWalk _ _ [] _ = 0
ghostWalk froms cond modes network = go froms cond (cycle modes) network
  where
    go _ _ [] _ = 0
    go fs p (m : ms) network' =
      let ss = map (\f -> step f m network') fs
       in if all p ss
            then 1
            else 1 + ghostWalk ss cond ms network

-- ghostWalk :: [String] -> (String -> Bool) -> [Mode] -> Network -> Int
-- ghostWalk froms cond modes network = head $ dropWhile (not . p) xs
--   where
--     subPaths = map (\f -> cycleWalk f modes network) froms
--     (pres, cycles') = unzip subPaths
--     cycles = map (map fst) cycles'
--     cyclesLen = map length cycles
--     zPos' = concatMap (findIndices cond) cycles
--     zPos = zipWith (+) (map length pres) zPos'
--     xs = scanl1 (+) $ repeat (minimum cyclesLen)
--     f' c z n = n `mod` c == z
--     fs = zipWith f' (map length cycles) zPos
--     p n = all (\fn -> fn n) fs

step :: String -> Mode -> Network -> String
step node mode net =
  let (left, right) = M.findWithDefault ("", "") node net
   in case mode of
        ModeLeft -> left
        ModeRight -> right

solvePart01 :: [String] -> Int
solvePart01 = (1 `subtract`) . length . uncurry (walk "AAA" "ZZZ") . parseInput

solvePart02 :: [String] -> Int
solvePart02 input =
  let (modes, net) = parseInput input
      fs = filter (\k -> last k == 'A') $ M.keys net
      stop k = last k == 'Z'
   in 0 -- ghostWalk fs stop modes net
