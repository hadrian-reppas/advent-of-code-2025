module Day09 (part1, part2) where

import Data.Array
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Set qualified as Set

parseInput :: String -> [(Int, Int)]
parseInput = map parseLine . lines
  where
    parseLine l = let [a, b] = splitOn "," l in (read a, read b)

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

maxArea :: ((Int, Int) -> (Int, Int) -> Int) -> [(Int, Int)] -> Int
maxArea _ [_] = 0
maxArea f (ij : ijs) = max (maximum $ map (f ij) ijs) (maxArea f ijs)

part1 :: String -> Int
part1 = maxArea area . parseInput

getUnique :: [(Int, Int)] -> ((Int, Int) -> Int) -> Array Int Int
getUnique points p =
  let vals = map p points ++ map ((+ 1) . p) points
      set = Set.fromList vals
   in listArray (1, Set.size set) (Set.toList set)

cellsBetween :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
cellsBetween ((i1, j1), (i2, j2)) =
  if i1 == i2
    then [(i1, j) | j <- [min j1 j2 .. max j1 j2]]
    else [(i, j1) | i <- [min i1 i2 .. max i1 i2]]

getBoundary :: [(Int, Int)] -> Set.Set (Int, Int)
getBoundary points =
  let points' = last points : points
   in Set.fromList $ concatMap cellsBetween (zip points' points)

indexOf :: Array Int Int -> Int -> Int
indexOf array value = let Just i = elemIndex value (elems array) in i + 1

bfs :: Set.Set (Int, Int) -> [(Int, Int)] -> Set.Set (Int, Int)
bfs seen [] = seen
bfs seen ((i, j) : stack) =
  if Set.member (i, j) seen
    then bfs seen stack
    else
      bfs
        (Set.insert (i, j) seen)
        ((i + 1, j) : (i, j + 1) : (i - 1, j) : (i, j - 1) : stack)

getSizes :: Array Int Int -> Array Int Int -> Array (Int, Int) Int
getSizes xs ys =
  array
    ((1, 1), (length xs - 1, length ys - 1))
    [ ((i, j), (xs ! (i + 1) - xs ! i) * (ys ! (j + 1) - ys ! j))
      | i <- [1 .. length xs - 1],
        j <- [1 .. length ys - 1]
    ]

getValidArray :: Set.Set (Int, Int) -> Int -> Int -> Array (Int, Int) Bool
getValidArray valid width height =
  array
    ((1, 1), (width, height))
    [ ((i, j), Set.member (i, j) valid)
      | i <- [1 .. width],
        j <- [1 .. height]
    ]

sum2d :: Array (Int, Int) Int -> Int -> Int -> Int -> Int -> Int
sum2d ps i1 i2 j1 j2 =
  ps ! (i2, j2)
    - ps ! (i1 - 1, j2)
    - ps ! (i2, j1 - 1)
    + ps ! (i1 - 1, j1 - 1)

validArea ::
  Array (Int, Int) Int ->
  Array (Int, Int) Int ->
  (Int, Int) ->
  (Int, Int) ->
  Int
validArea valid sizes (i1, j1) (i2, j2) =
  let (i1', i2') = (min i1 i2, max i1 i2)
      (j1', j2') = (min j1 j2, max j1 j2)
   in if sum2d valid i1' i2' j1' j2' == (i2' - i1' + 1) * (j2' - j1' + 1)
        then sum2d sizes i1' i2' j1' j2'
        else 0

prefixSum :: Array (Int, Int) a -> (a -> Int) -> Array (Int, Int) Int
prefixSum a f =
  let (_, (w, h)) = bounds a
      ps =
        array
          ((0, 0), (w, h))
          [ ((x, y), val x y)
            | x <- [0 .. w],
              y <- [0 .. h]
          ]
      val 0 _ = 0
      val _ 0 = 0
      val x y =
        f (a ! (x, y))
          + ps ! (x - 1, y)
          + ps ! (x, y - 1)
          - ps ! (x - 1, y - 1)
   in ps

part2 :: String -> Int
part2 input =
  let points = parseInput input
      xs = getUnique points fst
      ys = getUnique points snd
      points' = map (bimap (indexOf xs) (indexOf ys)) points
      boundary = getBoundary points'
      valid = bfs boundary [(length xs `div` 2, length ys `div` 3)]
      valid' = getValidArray valid (length xs) (length ys)
      sizes = getSizes xs ys
      validPs = prefixSum valid' fromEnum
      sizesPs = prefixSum sizes id
   in maxArea (validArea validPs sizesPs) points'
