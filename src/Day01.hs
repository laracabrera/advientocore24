module Day01 where

import Text.Regex.PCRE ((=~))

import Paths_aoc2024 (getDataFileName)

pattern :: String
pattern = "CGGTAC"

day01 :: IO ()
day01 = do
  input <- getDataFileName "day01-input.txt" >>= readFile
  let matches = input =~ pattern :: [[String]]
  print (length matches)
