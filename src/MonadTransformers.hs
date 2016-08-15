module MonadTransformers where

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe

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


-- Refactoring nested cases with maybe monad
getAnswer :: String -> IO (Maybe String)
getAnswer expected = do
  l <- getLine
  if l == expected
    then return $ Just l
    else return $ Nothing

display :: IO ()
display = do
  a <- getAnswer "a"
  case a of
    Nothing -> putStrLn "nope"
    Just x -> do
      b <- getAnswer x
      case b of
        Nothing -> putStrLn "nope"
        Just _ -> putStrLn "correct!"

-- refactor #2 with MaybeT
getAnswer2 :: String -> MaybeT IO String
getAnswer2 expected = MaybeT $ do
  l <- getLine
  if l == expected
    then return $ Just l
    else return $ Nothing

display2 :: IO ()
display2 = do
  y <- runMaybeT $ do a <- getAnswer2 "a"
                      b <- getAnswer2 a
                      return b
  case y of Nothing -> putStrLn "nope"
            (Just _) -> putStrLn "correct!"

-- refactor #3 with liftIO & Alternative
display3 :: IO ()
display3 = do
  _ <- runMaybeT $ do a <- getAnswer2 "a"
                      b <- getAnswer2 a
                      liftIO $ putStrLn "correct"
                   <|> do liftIO $ putStrLn "nope"
  return ()


-- refactor #4 with MonadPlus and fromMaybeT
getAnswer4 :: (MonadPlus m, MonadIO m) => String -> m String
getAnswer4 expected = mfilter (== expected) $ liftIO getLine

-- | Remove the `MaybeT` using a default value.
fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT onFail = maybe onFail return <=< runMaybeT

display4 :: IO ()
display4 = fromMaybeT (putStrLn "nope") $ do
  a <- getAnswer4 "a"
  b <- getAnswer4 a
  liftIO $ putStrLn "correct!"

-- TODO : find still other ways to refactor this 
