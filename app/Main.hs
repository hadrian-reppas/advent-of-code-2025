module Main where

import Day01 qualified
import Day02 qualified

run :: (Show a, Show b) => Int -> (String -> a) -> (String -> b) -> IO ()
run day part1 part2 = do
  input <- readFile $ "input/day" ++ show day ++ ".txt"
  putStrLn $ "part1: " ++ show (part1 input)
  putStrLn $ "part2: " ++ show (part2 input)

main :: IO ()
main = do
  run 1 Day01.part1 Day01.part2
  run 2 Day02.part1 Day02.part2
