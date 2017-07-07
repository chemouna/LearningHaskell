{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Deriving where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Configurator.Types
import Data.Data
import Data.Typeable

-- A free monad to which we pass a functor
data MiniIoF a = Terminate
  | PrintLine String a
  | ReadLine (String -> a)

-- type MiniIoF = Free MiniIoF

instance Functor MiniIoF where
  _ `fmap` Terminate = Terminate
  f `fmap` PrintLine s a = PrintLine s (f a)
  f `fmap` ReadLine g = ReadLine (f . g)

-- instead of writing it by hand ghc can do it for us
data MiniIoF2 a = Terminate2
  | PrintLine2 String a
  | ReadLine2 (String -> a)
  deriving Functor

-- for deriving Foldable and Traversable
data List a = Nil | Cons a (List a) deriving (Eq, Show, Functor)

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x $ foldr f z xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

data List2 a = Nil2 | Cons2 a (List2 a) deriving (Eq, Show, Functor, Foldable, Traversable, Typeable, Data)

data AppState

-- newtype App a = App { unApp :: ReaderT Config (StateT AppState IO) a }
--                  deriving (Monad, MonadReader Config, MonadState AppState, MonadIO)


