module Ex1 where

readInt :: String -> Int
readInt = read

solve :: String -> Int
solve str = solve2 (map readInt (lines str))


{- Using a filter finds if there is any occurrence of the matching int
  If so, multiplies it with it and returns. -}
solve1 :: [Int] -> Int
solve1 [] = 0
solve1 (i:ints)
  | filter (== 2020 - i) ints == [] = solve1 ints
  | otherwise = i * (2020 - i)

{- List comprehension, doesn't actually check that a /= b /= c -}
solve2 :: [Int] -> Int
solve2 ints = head [a * b * c | a <- ints, 
                                b <- tail ints, 
                                c <- tail (tail ints), 
                                a + b + c == 2020]