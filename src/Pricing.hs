
module Pricing where

{--
group of customers , price per group
into four or fewer (non-overlapping) groups.

maxSales [9,1,5,5,5,5,4,8,80] = 1*80 + 2*8 + 4*5 + 1*4 = 120

maxSales [17,50,2] =  1*17 + 1*50 + 1*2 = 69

maxSales [130,110,90,13,6,5,4,3,0] = 4*4 + 90 + 110 + 130 = 346 

-}


maxSales :: [Int] -> Int
maxSales = undefined
