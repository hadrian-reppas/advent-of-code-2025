module Day10 (part1, part2) where

import Data.Bits
import Data.List
import Data.List.Split
import Data.Sequence (Seq (..), (><))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import System.Process

parseLights :: String -> Int
parseLights "" = 0
parseLights ('#' : ls) = 2 * parseLights ls + 1
parseLights (_ : ls) = 2 * parseLights ls

parseSequence :: String -> [Int]
parseSequence = map read . splitOn "," . tail . init

parseButton :: String -> Int
parseButton button =
  let bits = map (shift 1) (parseSequence button)
   in foldl' (.|.) 0 bits

parseLightInfo :: String -> (Int, [Int])
parseLightInfo line =
  let parts = splitOn " " line
      lights = tail . init . head $ parts
      buttons = tail . init $ parts
   in (parseLights lights, map parseButton buttons)

bfs :: [Int] -> Seq (Int, Int) -> Set.Set Int -> Int
bfs _ ((0, d) :<| _) _ = d
bfs buttons ((x, d) :<| queue) seen =
  if Set.member x seen
    then bfs buttons queue seen
    else
      bfs
        buttons
        (queue >< Seq.fromList (map (\b -> (b `xor` x, d + 1)) buttons))
        (Set.insert x seen)

minLightPresses :: (Int, [Int]) -> Int
minLightPresses (target, buttons) =
  bfs buttons (Seq.singleton (target, 0)) Set.empty

part1 :: String -> Int
part1 = sum . map (minLightPresses . parseLightInfo) . lines

parseJoltageInfo :: String -> ([[Int]], [Int])
parseJoltageInfo line =
  let parts = splitOn " " line
      buttons = tail . init $ parts
      joltage = last parts
   in (map parseSequence buttons, parseSequence joltage)

minJoltagePresses :: ([[Int]], [Int]) -> IO Int
minJoltagePresses (buttons, target) =
  let n = length buttons - 1
      decls =
        concat
          [ "(declare-const a"
              ++ show i
              ++ " Int)\n(assert (>= a"
              ++ show i
              ++ " 0))\n"
            | i <- [0 .. n]
          ]
      constraint i t =
        "(assert (= "
          ++ show t
          ++ " (+"
          ++ concat [" a" ++ show j | j <- [0 .. n], i `elem` (buttons !! j)]
          ++ ")))\n"
      constraints = concat $ zipWith constraint [0 ..] target
      solve =
        "(minimize (+ "
          ++ (unwords ["a" ++ show i | i <- [0 .. n]])
          ++ "))\n(check-sat)\n(get-objectives)\n"
   in do
        writeFile "/tmp/aoc.z3" (decls ++ constraints ++ solve)
        stdout <- readProcess "z3" ["/tmp/aoc.z3"] ""
        pure $ (read . init . last . init . words) stdout

part2 :: String -> IO Int
part2 = fmap sum . mapM (minJoltagePresses . parseJoltageInfo) . lines
