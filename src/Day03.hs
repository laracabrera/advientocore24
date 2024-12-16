module Day03 where

import Paths_aoc2024 (getDataFileName)

import Numeric.LinearAlgebra (Matrix, fromLists, conv2, (!), rows, cols)

filtradoCuarentaYDos :: Matrix Double -> [(Int, Int)]
-- conv2 añade padding (filas y columnas adicionales, hay que ajustar los índices)
filtradoCuarentaYDos mat =
    [ (i-1, j-1) | i <- [2..nrows-3],
                   j <- [2..ncols-3],
                   let val = mat ! i ! j,
                   val > 42 ]
  where
    nrows = rows mat
    ncols = cols mat

day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  let inputInts = map (map (\c -> read [c] :: Double)) inputLines
      a = fromLists inputInts
      -- a = listArray ((0, 0), (length inputLines - 1, length (head inputLines) - 1)) (concat inputInts) :: Array (Int, Int) Int
      filtro = fromLists [[1.0, 1.0, 1.0], [1.0, 0.0, 1.0], [1.0, 1.0, 1.0]] :: Matrix Double
      b = conv2 filtro a
  print $ sum [a ! i ! j | (i, j) <- filtradoCuarentaYDos b]
 