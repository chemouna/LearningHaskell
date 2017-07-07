
module Maybe where

import Data.Int
import Data.List
import Data.Maybe
import Data.String
import Prelude
import Text.Read ( readMaybe )

-- mapMaybe
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

intsFromList :: [String] -> [Int]
intsFromList = mapMaybe readMaybeInt

intsFromList2 :: [String] -> [Int]
intsFromList2 xs = catMaybes $ map readMaybeInt xs
