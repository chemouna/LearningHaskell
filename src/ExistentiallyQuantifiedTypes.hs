{-# LANGUAGE ExistentialQuantification, RankNTypes #-}


module ExistentiallyQuantifiedTypes where

newtype Pair a b = Pair {runPair :: forall c. (a -> b -> c) -> c}

makePair :: a -> b -> Pair a b
makePair a b = Pair $ \f -> f a b
