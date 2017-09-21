module Hr_birthday_cake where

import Control.Applicative
import Control.Monad
import System.IO
import Data.List

solution :: [Int] -> Int
solution xs = (length (filter (\x -> x == m) xs)) +  (length (filter (\x -> x == n && x /= m) xs))
      where
        sxs = sort xs
        len = length xs
        m =  sxs !! (len - 1)
        n = sxs !! (len - 2)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    ar_temp <- getLine
    let ar = map read $ words ar_temp :: [Int]
    print $ solution ar

