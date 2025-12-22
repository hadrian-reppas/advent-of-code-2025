module Day11 (part1, part2) where

import Data.List
import Data.List.Split
import Data.Map qualified as Map

identity :: Int -> [[(Int, Int)]]
identity n = [[(i, 1)] | i <- [0 .. n - 1]]

dot :: [(Int, Int)] -> [(Int, Int)] -> Int
dot ((i, x) : xs) ((j, y) : ys)
  | i == j = x * y + dot xs ys
  | i < j = dot xs ((j, y) : ys)
  | otherwise = dot ((i, x) : xs) ys
dot _ _ = 0

mul :: [[(Int, Int)]] -> [[(Int, Int)]] -> [[(Int, Int)]]
mul rows cols =
  [ [ (c, dot row col)
      | (c, col) <- zip [0 ..] cols,
        dot row col /= 0
    ]
    | row <- rows
  ]

get :: [[(Int, Int)]] -> Int
get (((0, _) : (1, x) : _) : _) = x
get (((1, x) : _) : _) = x
get _ = 0

parseInput :: [String] -> String -> [(String, [String])]
parseInput remove input =
  ("out", [])
    : [ (k, filter (`notElem` remove) xs)
        | (k, xs) <- map parseLine (lines input),
          k `notElem` remove
      ]
  where
    parseLine line =
      let [a, b] = splitOn ": " line
       in (a, words b)

moveToFront :: String -> String -> [(String, [String])] -> [(String, [String])]
moveToFront u v graph =
  let (u', graph') = partition ((== u) . fst) graph
      (v', graph'') = partition ((== v) . fst) graph'
   in u' ++ v' ++ graph''

toMatrix :: String -> String -> [String] -> String -> [[(Int, Int)]]
toMatrix u v remove input =
  let graph = parseInput remove input
      graph' = moveToFront u v graph
      indices = Map.fromList $ zip (map fst graph') [0 ..]
   in [sort [(indices Map.! x, 1) | x <- xs] | (_, xs) <- graph']

countPathsRec :: [[(Int, Int)]] -> [[(Int, Int)]] -> Int
countPathsRec pow trans =
  if all null pow
    then 0
    else get pow + countPathsRec (mul pow trans) trans

countPaths :: String -> String -> [String] -> String -> Int
countPaths u v remove input =
  let matrix = toMatrix u v remove input
      ident = identity (length matrix)
   in countPathsRec ident (mul ident matrix)

part1 :: String -> Int
part1 = countPaths "you" "out" []

part2 :: String -> Int
part2 input =
  countPaths "svr" "out" [] input
    - countPaths "svr" "out" ["fft"] input
    - countPaths "svr" "out" ["dac"] input
    + countPaths "svr" "out" ["fft", "dac"] input
