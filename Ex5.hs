module Ex5 where

import Data.List
import Data.List.Split
import Text.Regex.Posix

solve :: String -> Int
solve seatlines = solve1 (lines seatlines)

solve1 :: [String] -> Int
solve1 seats = foldr max 0 (map seatId seats)

seatId :: String -> Int
seatId seat = 8*row + column
  where row = index (take 7 seat)
        column = index (drop 7 seat)

index :: String -> Int
index chars = sum (zipWith (\x y -> (topHalf x)*2^y) chars (reverse [0..length chars-1]))

topHalf :: Char -> Int
topHalf 'B' = 1
topHalf 'R' = 1
topHalf 'F' = 0
topHalf 'L' = 0
topHalf c = error ("the char:"++[c]++": caused the problem.")