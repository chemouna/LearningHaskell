module ZipWith where

{--

zipWith calls the given function pairwise on each member of both lists. So zipWith f [a,b,c] [x,y,z]
evaluates to [f a x, f b y, f c z]

-}

-- Examples

expl1 = [zipWith (*) [1,2,3] [3,2,2],
         zipWith (*) [3,5,6] [3,4,5],
         zipWith (*) [2,3,4] [5,4,3] ]

-- function that takes two lists and tries to divide each element of list A by the corresponding element
-- of list B. If the element in list B is 0, it should return Nothing,
-- otherwise it should return Just (a / b)
divlist :: Integral a => [a] -> [a] -> [Maybe a]
divlist = zipWith (\x y -> if (y /= 0) then Just (x `div` y) else Nothing)

-- divlist [3, 4, 6] [2, 0, 3]


-- fibonacci:
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- palindrome
palindrome xs = and $ zipWith (==) xs (reverse xs)

{--
Example Usage: Pascal Triangle
The root element is 1. Every other element is the sum of the one or two above it (diagonally left and
diagonally right).

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1

use is induction. The first row of the triangle is [1], and each row can be computed from the previous
row by adding the row shifted left, and the row shifted right:

--}

next xs = zipWith (+) ([0] ++ xs) (xs ++ [0])
pascal = iterate next [1]

next2 xs = [1] ++ zipWith (+) xs (tail xs) ++ [1]


{--
Given an array of numbers, nums, return an array of numbers products, where products[i] is the product of
all nums[j], j != i.

Input : [1, 2, 3, 4]
Output: [(2*3*4), (1*3*4), (1*2*4), (1*2*3)]
      = [120, 60, 40, 30]

do this in O(N) without using division.

Solution :
construct these two arrays then multiply each element with the corresponding to get the result:
{              1,         a[0],    a[0]*a[1],    a[0]*a[1]*a[2],  }
{ a[1]*a[2]*a[3],    a[2]*a[3],         a[3],                 1,  }

--}

otherProducts xs = zipWith (*) above below
  where
    above = scanl (*) 1 $ init xs
    below = tail $ scanr (*) 1 xs
