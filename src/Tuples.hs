

module Tuples where

import Data.List (groupBy, sort)
import qualified Data.Map as M

{-
[("Mary", 10), ("John", 45), ("Bradley", 30), ("Mary", 15), ("John", 10)]
and what I want to get is a list with also tuples where, if the name is the same, the numbers of those tuples should be added and, if not, that tuple must be part of the final list too, exemplifying:

[("Mary",25), ("John", 55), ("Bradley", 30)]
-}

solution :: [(String, Int)] -> [(String, Int)]
solution = map (helper . unzip) . groupBy (\ x y -> fst x == fst y) . sort
  where helper (names, vals) = (head names, sum vals)


solution' :: [(String, Int)] -> [(String, Int)]
solution' = M.toList . M.fromListWith (+)
