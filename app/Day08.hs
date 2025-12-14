module Day08 (part1, part2) where

import Data.Array
import Data.List
import Data.List.Split
import Data.Map (fromListWith)
import Data.Map qualified as M
import Data.Ord

parseInput :: String -> Array Int (Int, Int, Int)
parseInput input =
  let rows = lines input
   in listArray (1, length rows) $ map parseLine rows
  where
    parseLine l =
      let [x, y, z] = splitOn "," l
       in (read x, read y, read z)

distanceTuple :: Array Int (Int, Int, Int) -> Int -> Int -> (Int, Int, Int)
distanceTuple boxes i j =
  let (x1, y1, z1) = boxes ! i
      (x2, y2, z2) = boxes ! j
   in ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2, i, j)

union_ :: Array Int Int -> Int -> Int -> (Array Int Int, Bool)
union_ parents i j =
  let ips = reverse $ find_ parents i
      jps = reverse $ find_ parents j
      updatei = zip (drop 1 ips) (repeat $ head ips)
      updatej = zip jps (repeat $ head ips)
   in (parents // (updatei ++ updatej), head ips /= head jps)

find_ :: Array Int Int -> Int -> [Int]
find_ parents i =
  if parents ! i == i
    then [i]
    else i : find_ parents (parents ! i)

joinBoxes :: Int -> [(Int, Int, Int)] -> Array Int Int -> Array Int Int
joinBoxes 0 _ parents = parents
joinBoxes n ((_, i, j) : distances) parents =
  joinBoxes (n - 1) distances (fst $ union_ parents i j)

setup :: String -> (Array Int (Int, Int, Int), Array Int Int, [(Int, Int, Int)])
setup input =
  let boxes = parseInput input
      n = length boxes
      parents = listArray (1, n) [1 .. n]
      idxs = [(i, j) | i <- [1 .. n], j <- [1 .. i - 1]]
      distances = sort $ map (uncurry $ distanceTuple boxes) idxs
   in (boxes, parents, distances)

part1 :: String -> Int
part1 input =
  let (_, parents, distances) = setup input
      joined = joinBoxes 1000 distances parents
      counts = fromListWith (+) [(last $ find_ joined x, 1) | x <- elems joined]
      a : b : c : _ = sortBy (comparing Down) (M.elems counts)
   in a * b * c

joinAllBoxes :: Int -> [(Int, Int, Int)] -> Array Int Int -> (Int, Int)
joinAllBoxes circuits ((_, i, j) : distances) parents =
  let (parents', merged) = union_ parents i j
      circuits' = if merged then circuits - 1 else circuits
   in if circuits' == 1 then (i, j) else joinAllBoxes circuits' distances parents'

part2 :: String -> Int
part2 input =
  let (boxes, parents, distances) = setup input
      (i, j) = joinAllBoxes (length parents) distances parents
      (x1, _, _) = boxes ! i
      (x2, _, _) = boxes ! j
   in x1 * x2
