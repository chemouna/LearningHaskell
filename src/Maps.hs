
module Maps where

import Data.Maybe
import qualified Data.Map as M


-- update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]


-- expl of using the fact that Map is an instance of Foldable/Traversable

-- when to use Data.Map.Lazy or Data.Map.Strict ?

