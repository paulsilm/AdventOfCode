module Ex6 where

import Data.List
import Data.List.Split
import Text.Regex.Posix

az = ['a'..'z']

solve :: String -> Int
solve str = solve2 (splitOn "\n\n" str)

solve1 :: [String] -> Int
solve1 answers = sum (map countAnswers1 answers)

countAnswers1 :: String -> Int
countAnswers1 str = length (filter (\c -> elem c str) az)

solve2 :: [String] -> Int
solve2 answers = sum (map countAnswers2 answers)

countAnswers2 :: String -> Int
countAnswers2 str = length (filter (\c -> elemAll c (lines str)) (filter (\c -> elem c str) az))

elemAll :: Char -> [String] -> Bool
elemAll c [] = True
elemAll c (s:ss) = (elem c s) && elemAll c ss

fieldData :: String -> String -> String
fieldData field pass = head (words after) 
  where (_,_,after) = pass =~ field :: (String,String,String)
