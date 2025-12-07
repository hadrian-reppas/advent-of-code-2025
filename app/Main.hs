module Main where

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified

run :: (Show a, Show b) => Int -> (String -> a) -> (String -> b) -> IO ()
run day part1 part2 = do
  input <- readFile $ "input/day" ++ show day ++ ".txt"
  putStrLn $ "day " ++ show day ++ ":"
  putStrLn $ "  part 1: " ++ show (part1 input)
  putStrLn $ "  part 2: " ++ show (part2 input)

main :: IO ()
main = do
  run 1 Day01.part1 Day01.part2
  run 2 Day02.part1 Day02.part2
  run 3 Day03.part1 Day03.part2
  run 4 Day04.part1 Day04.part2
