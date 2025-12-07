module Day07 (part1, part2) where

import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

parseInput :: String -> [[Int]]
parseInput = map (map fst . filter ((== '^') . snd) . zip [0 ..]) . lines

splitBeam :: Set Int -> Int -> Set Int
splitBeam beams beam =
  Set.union (Set.delete beam beams) (Set.fromList [beam - 1, beam + 1])

update1 :: (Set Int, Int) -> Int -> (Set Int, Int)
update1 (beams, count) splitter =
  if Set.member splitter beams
    then (splitBeam beams splitter, count + 1)
    else (beams, count)

part1 :: String -> Int
part1 input =
  let Just start = elemIndex 'S' input
      splitters = parseInput input
   in snd $ foldl (foldl update1) (Set.singleton start, 0) splitters

updateCounts :: Map Int Int -> Int -> Map Int Int
updateCounts counts beam =
  let count = counts Map.! beam
      addCount count0 = Just $ fromMaybe 0 count0 + count
   in Map.alter addCount (beam + 1) $
        Map.alter addCount (beam - 1) $
          Map.delete beam counts

update2 :: Map Int Int -> Int -> Map Int Int
update2 beams splitter =
  if Map.member splitter beams
    then updateCounts beams splitter
    else beams

part2 :: String -> Int
part2 input =
  let Just start = elemIndex 'S' input
      splitters = parseInput input
   in sum $ Map.elems $ foldl (foldl update2) (Map.singleton start 1) splitters
