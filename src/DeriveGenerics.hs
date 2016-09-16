{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}

module DeriveGenerics where

import Data.Maybe
import GHC.Generics

data Valid e a = Error e | OK a
  deriving (Generic)


class GetError rep e | rep -> e where
  getError' :: rep a -> Maybe e

instance GetError f e => GetError (M1 i c f) e where
   getError' (M1 m1) = getError' m1

instance GetError l e => GetError (l :+: r) e where
   getError' (L1 l) = getError' l
   getError' (R1 _) = Nothing

instance GetError (K1 i e) e where
   getError' (K1 e) = Just e

getError :: (Generic (errorLike e a), GetError (Rep (errorLike e a)) e) => errorLike e a -> Maybe e
getError = getError' . from


