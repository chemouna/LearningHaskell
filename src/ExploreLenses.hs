{-# LANGUAGE TemplateHaskell #-}

module ExploreLenses where

import Control.Lens
import Numeric.Natural (Natural)

data Point = Point {
   _x, _y :: Double
  } deriving (Show)

data Monster = Monster {
  _monsterLocation :: Point
                       } deriving (Show)

makeLenses ''Point
makeLenses ''Monster

ogre = Monster (Point 0 0)

nat :: Prism' Integer Natural
nat = prism toInteger $ \ i ->
  if i < 0
     then Left i
     else Right (fromInteger i)
