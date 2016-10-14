module Loops where

-- perform an action multiple times based on a condition
performOnCond :: IO ()
performOnCond = loop 1
  where
    loop x | x < 150 = do
      putStr (show x ++ ", ")
      loop (x * 2)
    loop _ = return ()

-- a better way to write it
performOnCond2 :: IO ()
performOnCond2 = mapM_ print $ takeWhile (< 150) $ iterate (*2) 1

-- perform
b
