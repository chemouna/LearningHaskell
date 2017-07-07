
module ExploreFunctionsTuples where

import Control.Monad.State
import Control.Arrow
import Control.Applicative
import qualified Data.Set as S

boring :: Int -> (Int, String)
boring x = (x+1, show x)

lessBoring :: (b -> c) -> (b -> d) -> b -> (c, d)
lessBoring f g x = (f x, g x)

-- Creating tuple by passing `x` and `s` to `S.member` and `S.insert`:
tupleMemberInsert1 :: Ord a => a -> State (S.Set a) Bool
tupleMemberInsert1 x = state (\s -> (x `S.member` s, x `S.insert` s))

-- Applying first &&& trick:
tupleMemberInsert2 :: Ord a => a -> State (S.Set a) Bool
tupleMemberInsert2 x = state $ S.member x &&& S.insert x

-- Applying multi-arg trick for fun and profit:
tupleMemberInsert3 :: Ord a => a -> State (S.Set a) Bool
tupleMemberInsert3 = state . (liftA2 (&&&) S.member S.insert)


