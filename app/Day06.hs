module Day06 (part1, part2) where

import Data.List

parse :: [String] -> String -> [([String], Char)]
parse _ "" = []
parse nums (op : ops) =
  let k = length $ takeWhile (== ' ') ops
      grid = map (take k) nums
      nums' = map (drop (k + 1)) nums
   in (grid, op) : parse nums' (drop k ops)

parseInput :: String -> [([String], Char)]
parseInput input =
  let rows = lines input
   in parse (init rows) (last rows ++ " ")

parseGrid1 :: [String] -> [Int]
parseGrid1 = map (read . filter (/= ' '))

eval :: ([String] -> [Int]) -> ([String], Char) -> Int
eval parseGrid (grid, op) =
  let nums = parseGrid grid
   in if op == '+' then sum nums else product nums

part1 :: String -> Int
part1 input = sum $ map (eval parseGrid1) (parseInput input)

parseGrid2 :: [String] -> [Int]
parseGrid2 = parseGrid1 . transpose

part2 :: String -> Int
part2 input = sum $ map (eval parseGrid2) (parseInput input)
