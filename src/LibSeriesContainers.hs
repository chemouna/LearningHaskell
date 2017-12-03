
module LibSeriesContainers where

import Data.Map
import Data.Set
import Data.Foldable
import Data.Functor
import Control.Monad

{--
Containers: efficient general-purpose implementations of various basic immutable container types, maps, sets, graphs.

-}

-- lets assume we have a function that does some IO to give us a mapping from a Person to a Set of
-- their favourite Colours.
data Person
data Colour

peopleFavColours :: IO (Map Person (Set Colour))
peopleFavColours = undefined


-- find a Set of all favourite colours: we just take all the Sets of Colours for all people and "smash"
-- them together into a new set
-- "smashing" together sounds exactly like something a fold, perhaps with a Monoid instance…
  -- allFavColours :: IO (Set Colour)
-- allFavColours = Data.Set.fold <$> peopleFavColours




