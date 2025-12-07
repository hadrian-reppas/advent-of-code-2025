module Day02 (part1, part2) where

import Data.List.Split

parseRanges :: String -> [(Int, Int)]
parseRanges = map parseRange . splitOn ","
  where
    parseRange r = let [a, b] = splitOn "-" r in (read a, read b)

invalid1 :: Int -> Bool
invalid1 n =
  let s = show n
      k = length s `div` 2
   in 2 * k == length s && take k s == drop k s

invalid2' :: String -> Int -> Bool
invalid2' s m
  | m == length s = False
  | otherwise =
      let k = length s `div` m
       in m * k == length s
            && let chunks = chunksOf m s
                   invalid = m * k == length s && all (== head chunks) chunks
                in invalid || invalid2' s (m + 1)

invalid2 :: Int -> Bool
invalid2 n = invalid2' (show n) 1

sumInvalid :: (Int -> Bool) -> String -> Int
sumInvalid f input =
  let ranges = parseRanges input
      ids = concatMap (\t -> [fst t .. snd t]) ranges
   in sum $ filter f ids

part1 :: String -> Int
part1 = sumInvalid invalid1

part2 :: String -> Int
part2 = sumInvalid invalid2
