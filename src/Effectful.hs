{-# LANGUAGE FlexibleContexts #-}

module Effectfull where

import Data.Int
import Data.String
import Prelude

import System.IO
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.Trans.Reader (Reader)
import System.Environment
import Control.Monad.State

data AppConfig = AppConfig {
    logfile :: FilePath
   , version :: String
   , maxMessageLength :: Int
 } deriving (Show, Read)

-- First method: passing the config to each function
initLogFile :: String -> AppConfig -> IO Handle
initLogFile preamble config = do
  handle <- openFile (logfile config) WriteMode
  hPutStrLn handle (preamble ++ ", version" ++ version config)
  return handle

validateMessage :: String -> AppConfig -> Either String ()
validateMessage msg config =
     if (length msg > maxMessageLength config)
     then Left ("Message too long: " ++ msg)
     else Right ()

-- let's formalise needing a config

-- 1st method
type ConfigReader a = AppConfig -> a
initLogFileTS :: String -> ConfigReader (IO Handle)
initLogFileTS = initLogFile

validateMessageTS :: String -> ConfigReader (Either String ())
validateMessageTS = validateMessage

-- but we are still passing config function around -> let's illustrate it by validate the msg & only
-- when valide initialise the log file
validateAndInitLogTS :: String -> ConfigReader (IO (Maybe Handle))
validateAndInitLogTS prompt config =
  case validateMessage prompt config of
    Left err -> putStrLn ("Invalid prompt: " ++ err)
                >> return Nothing
    Right () -> Just <$> initLogFile prompt config


-- let's use a datatype to see if it simplifies for us
newtype CReader a = CReader { runCR :: AppConfig -> a }

initLogFileCR :: String -> CReader (IO Handle)
initLogFileCR p = CReader $ \c -> initLogFile p c

validateMessageCR :: String -> CReader (Either String ())
validateMessageCR m = CReader $ \c -> validateMessage m c

validateAndInitLogCR :: String -> CReader (IO (Maybe Handle))
validateAndInitLogCR m = CReader $ \c ->
  case runCR (validateMessageCR m) c of
    Left err -> putStrLn "Invalid init message" >> return Nothing
    Right () -> Just <$> runCR (initLogFileCR m) c

-- let's see an example usage for it
runCRWithConfig :: AppConfig -> IO Handle
runCRWithConfig config = do
  let result = runCR (validateAndInitLogCR "Hello CR") config
  -- IO action
  mh <- result
  case mh of Nothing -> error "Log file init failed."
             Just h -> return h

-- :k CReader =>  * -> *
-- let's try to create a Functor
instance Functor CReader where
  fmap f cr = CReader $ \c -> f (runCR cr c)

-- let's try to swap the boilerplate we used before with a functor
-- validateMessageF :: String -> CReader (Either String ())
-- validateMessageF m = fmap (validateMessage m) _

-- type hole says we need a type CReader AppConfig
askConfig :: CReader AppConfig
askConfig = CReader id

-- now let's use that to get all functor-ific
validateMessageF :: String -> CReader (Either String ())
validateMessageF m = fmap (validateMessage m) askConfig

initLogFileF :: String -> CReader (IO Handle)
initLogFileF p = fmap (initLogFile p) askConfig

validateAndInitLogF :: String -> CReader (IO (Maybe Handle))
validateAndInitLogF p = fmap doInit (validateMessageF p)
  where doInit :: Either String () -> (IO (Maybe Handle))
        doInit (Left err) = putStrLn ("Invalid prompt: "++ p)
                            >> return Nothing
        -- doInit (Right ()) = Just <$> initLogFileF -- Ouch! we need to pass config here we don't have it

-- Ok let's try with a monad instead

-- first the non point free version
-- instance Monad CReader where
--   return a = CReader $ \c -> a
--   a >>= f = CReader $ \c -> let a' = runCR a c
--                                 f' = f a'
--                                in runCR f' c

-- then the point free version
instance Monad CReader where
  -- return :: a -> CReader a
  return = CReader . const
  a >>= f = CReader $ \c -> runCR (f ((runCR a) c)) c

validateAndInitLogM :: String -> CReader (IO (Maybe Handle))
validateAndInitLogM p = do
  v <- validateMessageF p
  case v of
    Left err -> return (putStrLn ("Invalid prompt: "++ p)
                     >> return Nothing)
    Right () -> do
      h <- initLogFileF p
      return (fmap Just h)

-- this is mostly to get rid of the error raised by ghc: no instance for Applicative
-- instance Applicative CReader where
--   pure  = return
--   (<*>) = ap

-- let's create a correct implementation of Applicative 
instance Applicative CReader where
  pure = return
  (CReader f) <*> (CReader a) = CReader $ \c -> (f c) (a c)

-- Let's use the Reader (which is exactly like what we defined with CReader)
validateMsgRdr :: String -> Reader AppConfig (Either String ())
validateMsgRdr m = do
  max <- reader maxMessageLength -- this equivalent to : max <- maxMessageLength <$> ask
  if(length m > max)
    then return $ Left ("Message too long: " ++ m)
    else return $ Right ()

initLogFileRdr :: String -> Reader AppConfig (IO Handle)
initLogFileRdr p = do
  f <- reader logfile
  v <- reader version
  return $ do
    h <- openFile f WriteMode
    hPutStrLn h (p ++ "version: "++ v)
    return h

-- only problem with the above is that it's not running IO but returns an unevaluated IO action
-- Reader is a monad , IO is a monad => only to kind of have both run at the same time if with a
-- monad transformer -> ReaderT

initLogFileRT :: String -> ReaderT AppConfig IO Handle
initLogFileRT p = do
  f <- reader logfile
  v <- reader version
  h <- liftIO $ openFile f WriteMode
  liftIO $ hPutStrLn h (p ++ "version: "++ v)
  return h

validateMsgRT :: String -> ReaderT AppConfig IO (Either String ())
validateMsgRT m = vfun <$> reader maxMessageLength
  where vfun max | length m > max = Left ("Message too long: "++ m)
                 | otherwise = Right ()

-- validateMsgRT is bound to IO even though there's no IO going on in it -> let's make it more polymorphic
validateMessageRTM :: (Functor m, Monad m) => String -> ReaderT AppConfig m (Either String ())
validateMessageRTM m = vfun <$> reader maxMessageLength
  where vfun max | length m > max = Left ("Message too long: " ++ m)
                 | otherwise = Right ()

-- but we're still tied to ReaderT => let's use MonadReader to accept any kind of reader RWST or anything else
validateMessageMR :: (Functor m , MonadReader AppConfig m) => String -> m (Either String ())
validateMessageMR m = vfun <$> reader maxMessageLength
  where vfun max | length m > max = Left ("Message too long" ++ m)
                 | otherwise = Right ()

-- let's use the same polymorphic concept for IO too
initLogFileMR :: (MonadReader AppConfig m, MonadIO m) => String -> m Handle
initLogFileMR p = do
  f <- reader logfile
  v <- reader version
  h <- liftIO $ openFile f WriteMode
  liftIO $ hPutStrLn h (p ++ "version: "++ v)
  return h

-- Putting it all together
readConfig :: FilePath -> IO AppConfig
readConfig f = (fromTup . read) <$> (readFile f)
  where fromTup (a, b, c) = AppConfig a b c

main :: IO ()
main = do
  configPath <- head <$> getArgs
  configFile <- readConfig configPath
  runReaderT go configFile

go :: (Functor m, MonadReader AppConfig m, MonadIO m) => m ()
go = do
  h <- initLogFileMR "Starting"
  forever $ do
    liftIO $ putStr $ "Your Message: "
    m <- liftIO $ getLine
    v <- validateMessageMR m
    case v of
      (Left err) -> logMsg h $ "Invalid input: " ++ err
      (Right ()) -> logMsg h $ "Valid input"


logMsg :: (MonadIO m) => Handle -> String -> m ()
logMsg h = liftIO . hPutStrLn h
