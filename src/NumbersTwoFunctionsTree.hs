module NumbersTwoFunctionsTree where

{--
You are given a number, x0, and a target, xf. You are allowed two functions: f(x)=2x + 1 and g(x)=3x + 1.

Give either the minimal number of applications of f and g that you need in order to reach xf from x0,
or state that one cannot reach xf from x0.
-}

import System.Environment (getArgs)

data Tree = T { depth :: !Int,
                value :: !Int,
                l :: Tree,
                r :: Tree }
-- we keep the children lazy to be able to build infinite trees

mkTree :: Int -> Tree
mkTree = go 0
  where go d v = T { depth = d,
                     value = v,
                     l = go (d + 1) (2 * v + 1), -- apply f
                     r = go (d + 1) (3 * v + 1) -- apply g
                   }

bfs :: Tree -> [Tree]
bfs root =
  let
    nodes = root : children
    children = concatMap (\t -> [l t, r t]) nodes
  in nodes


{--
To get to a left child, apply f. To get to a right child, apply g. In this case the answer is equal to the depth of xf, where the root is x0. Now you just need to traverse the tree, starting from the root, until you find xf. Using the following observation:

4 ⋅ x > 3 ⋅ x + 1, ∀x > 1
we know that we can safely stop whenever we hit a node with depth d > 4 ⋅ x.

so we traverse the nodes produced by bfs until we hit either xf or a number that we know is too large
--}

distance :: Int -> Int -> Maybe Int
distance x0 xf = go nodes
  where
    nodes = bfs $ mkTree x0
    go (t:ts) | value t == xf    = Just $ depth t
              | value t > 4 * xf = Nothing
              | otherwise = go ts

main :: IO ()
main = do
   [x0, xf] <- (read <$>) <$> getArgs
   print $ distance x0 xf 
