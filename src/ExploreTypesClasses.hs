{-# LANGUAGE MultiParamTypeClasses #-}

module ExploreTypeClasses where


class CanAdd a where
  add :: a -> a -> a

instance CanAdd Int where
  add x y = x + y

instance CanAdd Float where
  add x y = x + y

instance CanAdd a => CanAdd (Maybe a) where
  add mx my =
    case (mx, my) of
      (Just x, Just y) -> Just (add x y)
      _ -> Nothing

class Mappable container where
  map :: (a -> b) -> container a -> container b

instance Mappable [] where
  map f list =
    case list of
      [] -> []
      x:xs -> f x : ExploreTypeClasses.map f xs

instance Mappable Maybe where
  map f maybe =
    case maybe of
      Just x -> Just (f x)
      Nothing -> Nothing
      



