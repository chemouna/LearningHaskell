{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskellSplices where

import TemplateHaskell (printf)

explPrintf = do
    putStrLn $ $(printf "Hello %s %%x%% %d %%x%%") "World" 12
    putStrLn $ $(printf "Hello %s %s %s %d") "Russian" "with" "Love" 5000
