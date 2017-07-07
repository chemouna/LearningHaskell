{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module RecordWildcards where

import Data.Aeson

data Worker = Worker
  {
    workerName :: String,
    workerPosition :: String,
    workerFirstYear :: String
  }

-- Without wildcards
-- instance ToJSON Worker where
--   toJSON w = object [ "name" .= workerName w
--                     , "position" .= workerPosition w
--                     , "first-year" .= workerFirstYear w
--                     ]

instance ToJSON Worker where
  toJSON Worker{..} = object [ "name" .= workerName
                             , "position" .= workerPosition
                             , "first-year" .= workerFirstYear
                             ]

instance FromJSON Worker where
  parseJSON = withObject "Worker" $ \o -> do
    workerName <- o .: "name"
    workerPosition <- o .: "position"
    workerFirstYear <- o .: "first-year"
    return Worker{..}

update :: Worker -> IO Worker
update Worker{..} = do
  workerPosition <- assignPosition
  workerFirstYear <- assignFirstYear
  return Worker{..}

assignPosition = undefined
assignFirstYear = undefined

-- expl 2 :
data C = C { a :: Int, b :: Int, c :: Int, d :: Int}
-- f(C { a = 1, b = b, c = c}) = b + c + d -- without wildcards
f (C {a = 1, ..}) = b + c + d



