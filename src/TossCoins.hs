{-# LANGUAGE FlexibleContexts #-}
module TossCoins where

import System.Random
import Control.Lens
import Control.Monad.Random
import Control.Applicative
import Control.Monad.Identity

-- | simulate tossing a coin 3 times
tossCoin3 :: Int -> (Bool, Bool, Bool)
tossCoin3 s =
  let (num1, gen1) = random (mkStdGen s) :: (Int, StdGen)
      (num2, gen2) = random gen1 :: (Int, StdGen)
      (num3, _) = random gen2 :: (Int, StdGen)

  in (convertToBool num1, convertToBool num2, convertToBool num3)
  where convertToBool x = x `mod` 2 == 0

-- | improvement 2 : use a map
map3Tuples :: (a -> b) -> (a, a, a) -> (b, b, b)
map3Tuples f (a1, a2, a3) = (f a1, f a2, f a3)

tossCoin3_2 :: Int -> (Bool, Bool, Bool)
tossCoin3_2 s =
  let (num1, gen1) = random (mkStdGen s) :: (Int, StdGen)
      (num2, gen2) = random gen1 :: (Int, StdGen)
      (num3, _) = random gen2 :: (Int, StdGen)

  in (map3Tuples convertToBool (num1, num2, num3))
  where convertToBool x = x `mod` 2 == 0

-- | improvement 3 : using lenses : map3tuple is realy not general what if we want to have a tuple with more than 3 elements)
tossCoin3_3 :: Int -> (Bool, Bool, Bool)
tossCoin3_3 s =
  let (num1, gen1) = random (mkStdGen s) :: (Int, StdGen)
      (num2, gen2) = random gen1 :: (Int, StdGen)
      (num3, _) = random gen2 :: (Int, StdGen)
  in over each ((0==) . (`mod` 2)) (num1, num2, num3)

-- | improvement 4 : we can make it more general and flexible by having as a param RandomGen instead of a seed for mkStdGen
tossCoin3_4 :: (RandomGen g) => g -> (Bool, Bool, Bool)
tossCoin3_4 g =
  let (num1, gen1) = random g
      (num2, gen2) = random gen1
      (num3, _) = random gen2
  in (num1, num2, num3)

-- | improvement 5 : using applicatives and MonadRandom to make it even better
-- tossCoin3_5 :: (RandomGen g, Random a) => g -> (a, a, a)
-- tossCoin3_5 = evalRand $ (,,) <$> random <*> random <*> random

