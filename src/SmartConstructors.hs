{-# LANGUAGE FlexibleInstances #-}

module SmartConstructors(
                    Resistor, -- this way we hide the constructor
                    metalResistor, -- and expose the constructor we want
                    MyType, createMyType,
                    MyType', createMyType') where

import Control.Exception
import Data.Time.Calendar

data Resistor = Metal Bands
              | Ceramic Bands
                deriving Show

type Bands = Int

-- to restrict this type constructor to bands of size of 4 to 8

metalResistor :: Bands -> Resistor
metalResistor n | n < 4 || n > 8 = error "Invalid number of resistor bands"
                | otherwise = Metal n

-- instead of hard coding the error we can use assert
metalResistor2 :: Bands -> Resistor
metalResistor2 n = assert (n >= 4 && n <= 8) $ Metal n

-- | another way to encode bounds at the type level using Type Arithmetic
data Z = Z
data S a = S a

class Card c where

instance Card Z where

instance (Card c) => Card (S c) where

class Card size => InBounds size where

instance InBounds (S (S (S (S Z)))) where                 -- four
instance InBounds (S (S (S (S (S Z))))) where             -- five
instance InBounds (S (S (S (S (S (S Z)))))) where         -- six
instance InBounds (S (S (S (S (S (S (S Z))))))) where     -- seven
instance InBounds (S (S (S (S (S (S (S (S Z)))))))) where -- eight

-- so with this check no need to encode the bounds

data Resistor' size = Resistor' deriving Show

resistor :: InBounds size => size -> Resistor' size
resistor _ = Resistor'

d0  = undefined :: Z -- with this `resistor d0` ->  No instance for (InBounds Z) because 0 isnt in the bounds
d3  = undefined :: S (S (S Z)) -- `resistor d3` ->  No instance for (InBounds Z)
d4  = undefined :: S (S (S (S Z))) -- Resistor because 4 is valid
d6  = undefined :: S (S (S (S (S (S Z))))) -- Resistor because 4 is valid

-- and to get even more information in the types lets separate Metal and Ceramic
newtype MetalResistor' = Metal' Bands
newtype CeramicResistor' = Ceramic' Bands


-- | Example 2
newtype Email = MkEmail String

-- create a smart constructor which does not promise that it can turn every string into an email. It promises only that it might -- be able to turn a string into an email, by returning a Maybe Email.
mkEmail :: String -> Maybe Email
mkEmail s = undefined


-- | Example 3: add restictions to types 
data Schedule = Schedule { startDate :: Day
    , endDate :: Day }
    deriving (Show)

mkSchedule :: Day -> Day -> Schedule
mkSchedule start end = assert (start < end) $ Schedule start end

-- | Example 4
-- I want to make a type MyType of integer triples. But not just Cartesian product of three Integer,
-- I want the type to represent all (x, y, z) such that x + y + z = 5
data MyType = MT { x :: Int, y :: Int, z :: Int}

createMyType :: Int -> Int -> MyType 
createMyType myX myY = MT { x = myX, y = myY, z = myX - myY }

-- what if we wanted to create a tuple with this condition x*x + y*y + z*z == 5
data MyType' = MT' { x' :: Int, y' :: Int, z' :: Int}

createMyType' :: Int -> Int -> Int -> Maybe MyType'
createMyType' myX myY myZ = assert ( myX*myX + myY*myY + myZ*myZ == 5) $ MT' myX myY myZ

