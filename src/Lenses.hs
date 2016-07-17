{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.State
import Control.Monad.Trans.Class


data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a}
makeLenses ''Foo

-- manipulating current state in a state monad
fresh :: MonadState Int m => m Int
fresh = id <+= 1

--

data Game = Game
    { _score :: Int
    , _units :: [Unit]
    , _boss  :: Unit
    } deriving (Show)

data Unit = Unit
    { _health   :: Int
    , _position :: Point
    } deriving (Show)

data Point = Point
    { _x :: Double
    , _y :: Double
    } deriving (Show)

-- approch of writing them by hand
-- score :: Lens' Game Int
-- score = lens _score (\game v -> game { _score = v })

-- units :: Lens' Game [Unit]
-- units = lens _units (\game v -> game { _units = v })

-- boss :: Lens' Game Unit
-- boss = lens _boss (\game v -> game { _boss = v })

-- health :: Lens' Unit Int
-- health = lens _health (\unit v -> unit { _health = v })

-- position :: Lens' Unit Point
-- position = lens _position (\unit v -> unit { _position = v })

-- x :: Lens' Point Double
-- x = lens _x (\point v -> point { _x = v })

-- y :: Lens' Point Double
-- y = lens _y (\point v -> point { _y = v })


-- let lens make them for us

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point

initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    }


strike :: StateT Game IO () 
strike = do
  lift $ putStrLn "*shink*"
  boss.health -=1

  
