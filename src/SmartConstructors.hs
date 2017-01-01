{-# LANGUAGE FlexibleInstances #-}

module SmartConstructors(
                    Resistor, -- this way we hide the constructor
                    metalResistor, -- and expose the constructor we want
                    Schedule,
                    mkSchedule',
                    MyType, createMyType,
                    MyType', createMyType',
                    Name, -- exports the type Name, but not the data constructor Name
                    nameFromString,
                    stringFromName) where

import Control.Exception
import Data.Time.Calendar
import Data.Maybe

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

-- or better instead of throwing an assert error
mkSchedule' :: Day -> Day -> Maybe Schedule
mkSchedule' s e
  | s < e = Just Schedule s e
  | otherwise = Nothing

-- | Example 4
-- I want to make a type MyType of integer triples. But not just Cartesian product of three Integer,
-- I want the type to represent all (x, y, z) such that x + y + z = 5
data MyType = MT { x :: Int, y :: Int, z :: Int}

createMyType :: Int -> Int -> MyType
createMyType myX myY = MT { x = myX, y = myY, z = myX - myY }

-- what if we wanted to create a tuple with this condition x*x + y*y + z*z == 5
data MyType' = MT' { x' :: Int, y' :: Int, z' :: Int}

createMyType' :: Int -> Int -> Int -> Maybe MyType'
createMyType' a b c
    | a + b + c == 5 = Just MT' { x' = a, y' = b, z' = c }
    | otherwise      = Nothing


-- | Example 5: forbid to create a Person with to long name (String containing no more than 10 letters)
data Name = Name String

-- this is the only way to create a Name
nameFromString :: String -> Maybe Name
nameFromString s | length s > 10 = Nothing
                 | otherwise     = Just (Name s)

-- this is the only way to access the contents of a Name
stringFromName :: Name -> String
stringFromName (Name s) = s

-- and if you wanted to throw an exception
nameFromString' :: String -> Name
nameFromString' = fromMaybe (error "attempted to construct an invalid Name") . nameFromString

-- but throwing isnt a good idea most of the time for example
-- only the first approach works well in this :

-- if we were using returning the error it would make things more complex
askUserForName :: IO Name
askUserForName
   = do putStr "What's your name? (10 chars max)  "
        s <- getLine
        case nameFromString s of
            Just n  -> return n
            Nothing -> askUserForName
