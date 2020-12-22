module Ex6 where

import Data.List
import Data.List.Split
import Text.Regex.Posix

az = ['a'..'z']

solve :: String -> Int
solve str = solve1 (splitOn "\n\n" str)

solve1 :: [String] -> Int
solve1 answers = sum (map countAnswers answers)

countAnswers :: String -> Int
countAnswers str = length (filter (\c -> elem c str) az)

fieldData :: String -> String -> String
fieldData field pass = head (words after) 
  where (_,_,after) = pass =~ field :: (String,String,String)
