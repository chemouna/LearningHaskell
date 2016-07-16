module MonadTransformers where

import Control.Applicative
import Control.Monad.Trans.Maybe

data MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
  fmap f (MaybeIO m) = MaybeIO $ (fmap.fmap) f m

instance Applicative MaybeIO where
  pure = MaybeIO . pure . Just
  MaybeIO f <*> MaybeIO m = MaybeIO $ liftA2 (<*>) f m

instance Monad MaybeIO where
  return = pure
  MaybeIO m >>= f = MaybeIO $ m >>= \x -> case x of
     Nothing  -> return $ Nothing
     Just val -> runMaybeIO $ f val

data User = User deriving Show

findById :: Int -> IO (Maybe User)
findById 1 = return $ Just User
findById _ = return Nothing

smartFindUsers :: Int -> Int -> IO (Maybe (User, User))
smartFindUsers x y = runMaybeIO $ do
  user1 <- MaybeIO $ findById x
  user2 <- MaybeIO $ findById y
  return (user1, user2)

-- let's use the generalized version of MaybeIO : MaybeT

transformerFindUsers :: Int -> Int -> IO (Maybe (User, User))
transformerFindUsers x y = runMaybeT $ do
  user1 <- MaybeT $ findById x
  user2 <- MaybeT $ findById y
  return (user1, user2)
