module Ex3 where

solve :: String -> Int
solve = solve2 [(1,1),(3,1),(5,1),(7,1),(1,2)]

solve1 :: String -> Int
solve1 str = count (lines str) (0, 0) (3,1) 0

solve2 :: [(Int,Int)] -> String -> Int
solve2 moves str = product (map (\x -> count (lines str) (0,0) x 0) moves)

count :: [String] -> (Int, Int) -> (Int, Int) -> Int -> Int
count [] _ _ cnt = cnt
count (l:lns) (column, 0) (rightMove, downMove) cnt
  | equalsAt column '#' l = count lns (newRI,downMove-1) (rightMove, downMove) (cnt + 1)
  | otherwise = count lns (newRI,downMove-1) (rightMove, downMove) cnt
  where newRI = (column+rightMove) `mod` ((length l)-1)
count (_:lns) (column, rowsToGo) (rightMove, downMove) cnt = count lns (column, rowsToGo-1) (rightMove, downMove) cnt 

equalsAt :: Int -> Char -> String -> Bool
equalsAt _ _ [] = False
equalsAt 0 c (c2:_) = c == c2
equalsAt i c (_:cs) = equalsAt (i-1) c cs