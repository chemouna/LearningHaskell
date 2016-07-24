{-# LANGUAGE TemplateHaskell #-}

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
