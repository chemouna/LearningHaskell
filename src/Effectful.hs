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
