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

