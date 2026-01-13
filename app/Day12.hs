module Day12 (part1, part2) where

import Data.List
import Data.List.Split
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Process

type Shape = [(Int, Int)]

type Region = (Int, Int, [Int])

parseShape :: [String] -> Shape
parseShape shape =
  sort
    [ (x, y)
      | (y, row) <- zip [-1, 0, 1] shape,
        (x, cell) <- zip [-1, 0, 1] row,
        cell == '#'
    ]

parseRegion :: String -> Region
parseRegion region =
  let [lhs, rhs] = splitOn ": " region
      [w, h] = splitOn "x" lhs
   in (read w, read h, map read (words rhs))

transform :: Shape -> [Shape]
transform r0 =
  let r1 = rotate r0
      r2 = rotate r1
      r3 = rotate r2
      f0 = reflect r0
      f1 = rotate f0
      f2 = rotate f1
      f3 = rotate f2
   in Set.toList (Set.fromList [r0, r1, r2, r3, f0, f1, f2, f3])
  where
    rotate s = sort [(y, -x) | (x, y) <- s]
    reflect s = sort [(-x, y) | (x, y) <- s]

toInt :: String -> String
toInt b = "(ite " ++ b ++ " 1 0)"

sumBools :: [String] -> String
sumBools [] = "0"
sumBools xs = "(+ " ++ unwords (map toInt xs) ++ ")"

eqCount :: [String] -> Int -> String
eqCount vars k = "(assert (= " ++ sumBools vars ++ " " ++ show k ++ "))\n"

leq1 :: [String] -> String
leq1 vars = "(assert (<= " ++ sumBools vars ++ " 1))\n"

makeSMT :: [Shape] -> Int -> Int -> [Int] -> String
makeSMT shapes w h counts =
  let build ss =
        let go _ [] = ([], [])
            go i (sh : rest) =
              let local =
                    [ ( "x" ++ show j,
                        [(x + dx, y + dy) | (dx, dy) <- t]
                      )
                      | t <- transform sh,
                        y <- [1 .. h - 2],
                        x <- [1 .. w - 2],
                        let j = i + (((tIndex t sh) * (h - 2) + (y - 1)) * (w - 2) + (x - 1))
                    ]
                  localVars = map fst local
                  nextI = i + length local
                  (restPlacements, restGroups) = go nextI rest
               in (local ++ restPlacements, localVars : restGroups)
            tIndex t sh = case elemIndex t (transform sh) of
              Just ix -> ix
              Nothing -> 0
         in go 0 ss
      (placements, shapeVars) = build shapes
      allVars = map fst placements
      cellMap =
        Map.fromListWith
          (++)
          [ ((cx, cy), [v])
            | (v, cells) <- placements,
              (cx, cy) <- cells
          ]
      decls = concat ["(declare-const " ++ v ++ " Bool)\n" | v <- allVars]
      countConstraints =
        concat
          [ eqCount vars k
            | (k, vars) <- zip counts shapeVars
          ]
      cellConstraints =
        concat
          [ leq1 vars
            | vars <- Map.elems cellMap
          ]
      solve = "(check-sat)\n"
   in decls ++ countConstraints ++ cellConstraints ++ solve

runZ3Sat :: String -> IO Bool
runZ3Sat smt = do
  writeFile "/tmp/aoc.z3" smt
  stdout <- readProcess "z3" ["/tmp/aoc.z3"] ""
  pure $ case lines stdout of
    ("sat" : _) -> True
    _ -> False

parseInput :: String -> ([Shape], [(Int, Int, [Int])])
parseInput input =
  let blocks = splitOn "\n\n" input
      regionsText = last blocks
      shapesText = init blocks
      shapes = map (parseShape . drop 1 . lines) shapesText
      regions =
        map parseRegion
          . filter (not . null)
          . lines
          $ regionsText
   in (shapes, regions)

part1 :: String -> IO Int
part1 input = do
  let (shapes, regions) = parseInput input
  sats <-
    mapM
      (\(w, h, counts) -> runZ3Sat (makeSMT shapes w h counts))
      regions
  pure (length (filter id sats))

part2 :: String -> Int
part2 _ = 0
