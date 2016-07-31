
module ExploreFunctionsTuples where

boring :: Int -> Int -> (Int, String)
boring x = (x+1, show x)

lessBoring :: (b -> c) -> (b -> d) -> b -> (c, d)
lessBoring f g x = (f x, g x)



