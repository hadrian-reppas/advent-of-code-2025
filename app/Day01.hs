module Day01 (part1, part2) where

parseMove :: String -> Int
parseMove ('R' : n) = read n
parseMove ('L' : n) = -read n

countZeros :: Int -> [String] -> Int
countZeros _ [] = 0
countZeros pos (move : moves) =
  let newPos = (pos + parseMove move) `mod` 100
      tailCount = countZeros newPos moves
   in if newPos == 0 then 1 + tailCount else tailCount

part1 :: String -> Int
part1 = countZeros 50 . lines

expand :: [String] -> [String]
expand [] = []
expand ((c : n) : moves) = replicate (read n) (c : "1") ++ expand moves

part2 :: String -> Int
part2 = countZeros 50 . expand . lines
