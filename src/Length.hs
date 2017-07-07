module Length where

-- Different implementations of length

-- Maps each element to a 1, then sums up all the elements
len :: [a] -> Integer
len = sum . map (\_ -> 1)

-- Adds up all the heads (recursively) until the list is empty
len' :: [a] -> Integer
len' [] = 0
len' (_:xs) = 1 + len' xs

-- Extract into a fold using this technique: [ [] := v, (:) := f ]
-- In the case of length, it requires one more step since we don't
-- use the head of the list in the recursive computation
len'' :: [a] -> Integer
len'' = foldr (\_ x -> x + 1) 0
