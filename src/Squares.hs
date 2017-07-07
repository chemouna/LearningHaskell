
module Squares where

isSquare x = let x' = truncate $ sqrt (fromIntegral x :: Double) in x'*x' == x

-- isSquare2 
