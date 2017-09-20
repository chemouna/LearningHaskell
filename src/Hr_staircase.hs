module Hr_staircase where

import Control.Applicative
import Control.Monad
import System.IO


{-
   #
  ##
 ###
####
-}

solution :: Int -> [String]
solution n = 

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    print n
