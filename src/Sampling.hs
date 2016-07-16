
module Sampling where

import Control.Monad.State
import System.Random (StdGen, mkStdGen, random)
import Control.Applicative ((<$>))

type R a = State StdGen a

