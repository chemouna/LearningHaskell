
module PascalTriangle where

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 1
pascal row col
  | row < col = error "Error "
  | col == row = 1
  | otherwise = pascal (row - 1) col + pascal (row - 1) (col - 1)

