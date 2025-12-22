module Main where

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Day08 qualified
import Day09 qualified
import Day10 qualified
import Day11 qualified

run :: Int -> (String -> IO Int) -> (String -> IO Int) -> IO ()
run day part1 part2 = do
  input <- readFile $ "input/day" ++ show day ++ ".txt"
  putStrLn $ "day " ++ show day ++ ":"
  result1 <- part1 input
  putStrLn $ "  part 1: " ++ show result1
  result2 <- part2 input
  putStrLn $ "  part 2: " ++ show result2

main :: IO ()
main =
  mapM_ (\(d, (p1, p2)) -> run d p1 p2) $
    zip
      [1 ..]
      [ (pure . Day01.part1, pure . Day01.part2),
        (pure . Day02.part1, pure . Day02.part2),
        (pure . Day03.part1, pure . Day03.part2),
        (pure . Day04.part1, pure . Day04.part2),
        (pure . Day05.part1, pure . Day05.part2),
        (pure . Day06.part1, pure . Day06.part2),
        (pure . Day07.part1, pure . Day07.part2),
        (pure . Day08.part1, pure . Day08.part2),
        (pure . Day09.part1, pure . Day09.part2),
        (pure . Day10.part1, Day10.part2),
        (pure . Day11.part1, pure . Day11.part2)
      ]
