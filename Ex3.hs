module Ex3 where

solve :: String -> Int
solve = solve2 [(1,1),(3,1),(5,1),(7,1),(1,2)]

solve1 :: String -> Int
solve1 str = count (lines str) (0, 0) (3,1) 0

solve2 :: [(Int,Int)] -> String -> Int
solve2 moves str = foldr (*) 1 (map (\x -> count (lines str) (0,0) x 0) moves)

count :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Int
count [] _ _ cnt = cnt
count (l:lns) (ri, 0) (right, down) cnt
  | equalsAt ri '#' l = count lns (newRI,down-1) (right, down) (cnt + 1)
  | otherwise = count lns (newRI,down-1) (right, down) cnt
  where newRI = (ri+right) `mod` ((length l)-1)
count (_:lns) (ri, di) (right, down) cnt = count lns (ri, di-1) (right, down) cnt 

equalsAt :: Int -> Char -> String -> Bool
equalsAt _ _ [] = False
equalsAt 0 c (c2:_) = c == c2
equalsAt i c (_:cs) = equalsAt (i-1) c cs