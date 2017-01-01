{-# LANGUAGE GADTs #-}
module Screencast1 where

-- Type constructors

-- Data Constructor (Nullary Type, Unary Type, Type with parameters)

-- Nullary Type
data Bool = True | False

-- Unary Type
data Tree a = Tip | Node a (Tree a)(Tree a)

-- data Tree Int = Tip | Node Int (Tree Int) (Tree Int)
-- data Tree String = Tio | Node String (Tree String) (Tree String)

-- data Either = Left | Right

-- Left :: forall b a. a -> Either a b

-- data Car = Car {
--               company :: String,
--               model :: String,
--               year :: Int
--             }

data Car a b c = Car {
                     company :: a,
                     model :: b,
                     year :: c
                     }

-- Car String String Int

type Vect2d = (Float, Float)

-- data Obj2d = Rect Float | Float
--                          | Translate Vect2d Obj2d

data Obj2d where
  
  Rect :: Float -> Float -> Obj2d
  Translate :: Vect2d -> Obj2d 

depth Tip = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)


