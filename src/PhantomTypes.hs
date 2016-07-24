{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module PhantomTypes where

import Data.Char

data FormData a = FormData String

changeType :: FormData a -> FormData b
changeType (FormData str) = FormData str

data Validated = Validated ()
data Unvalidated = Unvalidated ()

-- users with just string -> create unvalidated values
formData :: String -> FormData Unvalidated
formData str = FormData str

-- result of validate is either nothing or a validated result
validate :: FormData Unvalidated -> Maybe (FormData Validated)
validate (FormData str) = undefined

-- can only accept validate data
useData :: FormData Validated -> IO ()
useData str = undefined

--
liftStringFn :: (String -> String) -> (FormData a -> FormData a)
liftStringFn fn (FormData str) = FormData (fn str)

dataToUpper :: FormData a -> FormData a
dataToUpper = liftStringFn (map toUpper)

-- create conditional behavior with information nonexistent at runtime
class Sanitise a where
  sanitise :: FormData a -> FormData Validated

instance Sanitise Validated where
  sanitise = id

instance Sanitise Unvalidated where
  sanitise (FormData str) = FormData (filter isAlpha str)

-----------------
-- | Example 2
-----------------

newtype Distance a = Distance Double deriving (Num, Show)
data Kilometer
data Mile

marathonDistance :: Distance Kilometer
marathonDistance = Distance 42.25

distanceKmToMilles :: Distance Kilometer -> Distance Mile
distanceKmToMilles (Distance km) = Distance (0.62 * km)

marathonDistanceInMiles :: Distance Mile
marathonDistanceInMiles = distanceKmToMilles marathonDistance

-- using DataKind to restrict to only a set of units
data LengthUnit = Kilometer | Mile

newtype Distance2 (a :: LengthUnit) = Distance2 Double
  deriving (Num, Show)

marathonDistance2 :: Distance2 'Kilometer
marathonDistance2 = Distance2 42.25

distanceKmToMilles2 :: Distance2 'Kilometer -> Distance2 'Mile
distanceKmToMilles2 (Distance2 km) = Distance2 (0.62 * km)

marathonDistanceInMiles2 :: Distance2 'Mile
marathonDistanceInMiles2 = distanceKmToMilles2 marathonDistance2
