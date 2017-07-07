{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}


module ExistentiallyQuantifiedTypes where

newtype Pair a b = Pair {runPair :: forall c. (a -> b -> c) -> c}

makePair :: a -> b -> Pair a b
makePair a b = Pair $ \f -> f a b

--

data Foo = forall a. Show a => MakeFoo a a

-- MakeFoo :: Show a => a -> a -> Foo

-- test1 :: Show s => s
-- test1 = "foobar"

test2 :: Num a => a
test2 = 3

-- test3 :: forall s. Show s => s
-- test3 = "asd"

