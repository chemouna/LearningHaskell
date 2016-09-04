module Finding where

-- find the number of non overlapping occurences if a sub in a str
import Data.Text hiding (length)

countSubStrs :: String -> String -> Int
countSubStrs str sub = length $ breakOnAll (pack sub) (pack str)

displayCount :: IO ()
displayCount = do
  print $ countSubStrs "the three truths" "th"
  print $ countSubStrs "ababababab" "abab"
