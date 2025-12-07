module Day03 (part1, part2) where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (maximumBy)

maxJoltage :: Int -> String -> Int
maxJoltage 0 _ = 0
maxJoltage n s =
  let cmp = compare `on` \t -> (snd t, -fst t)
      (i, d) = maximumBy cmp $ zip [1 .. length s - n + 1] s
   in 10 ^ (n - 1) * digitToInt d + maxJoltage (n - 1) (drop i s)

part1 :: String -> Int
part1 = sum . map (maxJoltage 2) . lines

part2 :: String -> Int
part2 = sum . map (maxJoltage 12) . lines
