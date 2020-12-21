module Ex2 where

import Data.List.Split
import Text.Regex.Posix

readPW :: String -> (Int,Int,Char,String)
readPW ln = (read min, read max, c, pw)
  where (_,_,_,(min:max:[c]:pw:_)) = readThePw ln

readThePw :: String -> (String,String,String,[String])
readThePw ln = ln =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)"

solve :: String -> Int
solve str = length (filter isValid2 (map readPW (lines str)))

isValid :: (Int,Int,Char,String) -> Bool
isValid (min, max, c, pw) = len <= max && len >= min
  where len = length (filter (==c) pw)

isValid2 :: (Int,Int,Char,String) -> Bool
isValid2 (i1, i2, c, pw) = (equalsAt (i1-1) c pw) /= (equalsAt (i2-1) c pw)

equalsAt :: Int -> Char -> String -> Bool
equalsAt _ _ [] = False
equalsAt 0 c (c2:_) = c == c2
equalsAt i c (_:cs) = equalsAt (i-1) c cs