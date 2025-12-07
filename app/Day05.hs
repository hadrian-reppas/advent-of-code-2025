module Day05 (part1, part2) where

import Data.List
import Data.List.Split

parseInput :: String -> ([(Int, Int)], [Int])
parseInput input =
  let [ranges, ids] = splitOn "\n\n" input
   in (map parseRange $ lines ranges, map read $ lines ids)
  where
    parseRange range = let [l, u] = splitOn "-" range in (read l, read u)

part1 :: String -> Int
part1 input =
  let (ranges, ids) = parseInput input
      fresh i = any (\r -> fst r <= i && i <= snd r) ranges
   in length $ filter fresh ids

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ((l1, u1) : (l2, u2) : rs) =
  if l2 <= u1
    then mergeRanges ((l1, max u1 u2) : rs)
    else (l1, u1) : mergeRanges ((l2, u2) : rs)
mergeRanges l = l

part2 :: String -> Int
part2 input =
  let (ranges, _) = parseInput input
      merged = mergeRanges (sort ranges)
   in sum $ map (\r -> snd r - fst r + 1) merged
