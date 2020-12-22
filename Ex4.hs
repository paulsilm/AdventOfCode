module Ex4 where

import Data.List
import Data.List.Split
import Text.Regex.Posix

reqs = ["byr:","iyr:", "eyr:", "hgt", "hcl:", "ecl:", "pid"]

solve :: String -> Int
solve str = solve2 (splitOn "\r\n\r\n" str)

solve1 :: [String] -> Int
solve1 passports = length (filter isValid1 passports)

isValid1 :: String -> Bool
isValid1 pass = all (pass =~) reqs


solve2 :: [String] -> Int
solve2 passports = length (filter isValid2 passports)


isValid2 :: String -> Bool
isValid2 pass = isValid1 pass && 
                  isValidByr pass &&
                  isValidIyr pass &&
                  isValidEyr pass &&
                  isValidHgt pass &&
                  isValidEcl pass &&
                  isValidHcl pass &&
                  isValidPid pass
                
isValidByr pass = yr <= 2002 && yr >= 1920
  where yr = read (fieldData "byr:" pass)

isValidIyr pass = yr <= 2020 && yr >= 2010
  where yr = read (fieldData "iyr:" pass)

isValidEyr pass = yr <= 2030 && yr >= 2020
  where yr = read (fieldData "eyr:" pass)

isValidHgt pass 
  | heightNumber == "" = False
  | unit == "cm" = hgt <= 193 && hgt >= 150
  | unit == "in" = hgt <= 76 && hgt >= 59
  | otherwise = False
  where heightData = fieldData "hgt:" pass
        (heightNumber,unit,_) = heightData =~ "(cm|in)" :: (String,String,String)
        hgt = read heightNumber :: Int

isValidHcl pass = (fieldData "hcl:" pass) =~ "#[0-9a-f]{6}" :: Bool

isValidEcl pass = or (map (==color) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
  where color = fieldData "ecl:" pass

isValidPid pass = (id =~ "[0-9]{9}" :: Bool) && length id == 9
  where id = fieldData "pid:" pass

fieldData :: String -> String -> String
fieldData field pass = head (words after) 
  where (_,_,after) = pass =~ field :: (String,String,String)
