module KnuthMorrisPratt where

import Data.List

data KMP a = KMP
      { done :: Bool
      , next :: (a -> KMP a)
      }

as `isSubstringOf` bs = any (as `isPrefixOf`) (tails bs)

makeTable :: Eq a => [a] -> KMP a
makeTable xs = table
   where table = makeTable' xs (const table)

makeTable' []     failure = KMP True failure
makeTable' (x:xs) failure = KMP False test
   where  test  c = if c == x then success else failure c
          success = makeTable' xs (next (failure x))

isSubstringOf2 :: Eq a => [a] -> [a] -> Bool
isSubstringOf2 as bs = match (makeTable as) bs
   where  match table []     = done table
          match table (b:bs) = done table || match (next table b) bs

isSubstringOf3 as bs = any done $ scanl next (makeTable as) bs


prop_isSubstringOf :: [Bool] -> [Bool] -> Bool
prop_isSubstringOf as bs = (as `isSubstringOf` bs) == (as `isSubstringOf2` bs)

-- http://okmij.org/ftp/Haskell/KMP-deptype.hs
