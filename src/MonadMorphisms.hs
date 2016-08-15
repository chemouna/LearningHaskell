{-# LANGUAGE RankNTypes, TypeOperators #-}

module MonadMorphisms where

type m :-> n = forall a . m a -> n a




