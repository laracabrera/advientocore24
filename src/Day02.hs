{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day02 where

import Paths_aoc2024 (getDataFileName)
import Data.Char (isDigit, digitToInt)
import Data.HashMap (fromList, Map, lookup)
import Data.Maybe

procesarLinea :: String -> Integer
procesarLinea l = saltar (dropWhile (not . isDigit) l)

saltar :: String -> Integer
saltar (x:xs) | isDigit x = saltar (drop (digitToInt x - 1) xs)
              | otherwise = Data.Maybe.fromMaybe 0 (Data.HashMap.lookup x valores)
saltar [] = 0

valores :: Data.HashMap.Map Char Integer
valores = fromList $ zip (['a'.. 'z'] ++ ['A' .. 'Z']) [1 .. 52]
--valores = fromList [(k, v)| k <- ['a'.. 'z'] ++ ['A' .. 'Z'], v <- [1 .. 52]]

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  print $ sum (map procesarLinea inputLines)
