module Learning.GhcBase where

import Data.Either

min :: Ord a => a -> a -> Bool
min = (<)

-- Either
let s = Left "foo" :: Either String Int
let n = Right 3 :: Either String Int

--fmap (*2) s
--Left "foo"
--fmap (*2) n
--Right 6