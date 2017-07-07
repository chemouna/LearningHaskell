{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls, UndecidableInstances #-}

module TypeLevelPgm where

--class Plus m n r

data Z
data S n

-- instance Plus Z n n
-- instance (Plus m n r) => Plus (S m) n (S r)

class Plus m n r | m n -> r
instance Plus Z n n
instance (Plus m n r) => Plus (S m) n (S r)

-- using Type Families
type family Plus2 m n :: *
type instance Plus2 Z n = n
type instance Plus2 (S m) n = S (Plus2 m n)
