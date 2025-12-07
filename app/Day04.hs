module Day04
  ( part1,
    part2,
  )
where

import Data.Array

get :: Array (Int, Int) Bool -> (Int, Int) -> Bool
get grid (r, c) =
  let ((lr, lc), (ur, uc)) = bounds grid
   in lr <= r && r <= ur && lc <= c && c <= uc && grid ! (r, c)

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

removable :: Array (Int, Int) Bool -> [(Int, Int)]
removable grid =
  let deltas = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]
      count i = length $ filter (get grid . plus i) deltas
   in filter (\i -> get grid i && count i < 4) (indices grid)

parseGrid :: String -> Array (Int, Int) Bool
parseGrid input =
  let rows = lines input
      u = (length rows, length (head rows))
   in listArray ((1, 1), u) (map (== '@') (concat rows))

part1 :: String -> Int
part1 = length . removable . parseGrid

remove :: Array (Int, Int) Bool -> [(Int, Int)] -> Array (Int, Int) Bool
remove grid coords = grid // zip coords (repeat False)

removeRec :: Array (Int, Int) Bool -> Array (Int, Int) Bool
removeRec grid =
  let grid' = remove grid (removable grid)
   in if grid == grid' then grid else removeRec grid'

part2 :: String -> Int
part2 input =
  let before = parseGrid input
      after = removeRec before
   in length $ filter (uncurry (/=)) $ zip (elems before) (elems after)
