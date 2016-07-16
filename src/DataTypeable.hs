{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- reference : http://chrisdone.com/posts/data-typeable

module DataTypeable where

import           Data.Data
import           Data.Typeable
import           Data.Generics.Aliases


data X = X { foo :: Int, bar :: Char } deriving (Typeable,Data)

char :: Typeable a => a -> String
char x = case cast x of
                  Just (x :: Char) -> show x
                  Nothing -> "unknown"

-- exple of use for dataTypeOf and dataTypeConstrs
-- let l = Left () :: Either ()()
-- dataTypeOf l
-- dataTypeConstrs (dataTypeOf l)


-- dataTypeConstrs (dataTypeOf (Nothing :: Maybe ())) --> [Nothing,Just]
-- indexConstr (dataTypeOf (Nothing :: Maybe ())) 1 --> Nothing
-- indexConstr (dataTypeOf (Nothing :: Maybe ())) 2 --> Just

-- isAlgType (dataTypeOf (Just 'a')) --> True
-- isAlgType (dataTypeOf 'a') --> False


-- toConstr (Nothing :: Maybe Char)
-- toConstr (Left () :: Either () ())
-- toConstr (Just 'a') == toConstr (Nothing :: Maybe Char)

-- constrType (toConstr (Just 'a')) --> DataType {tycon = "Prelude.Maybe", datarep = AlgRep [Nothing,Just]}


-- constrFields (toConstr (X 0 'a')) --> ["foo","bar"]

gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS) where
  render t
    | isTuple = showChar '('
              . drop 1
              . commaSlots
              . showChar ')'
    | isNull = showString "[]"
    | isList = showChar '['
             . drop 1
             . listSlots
             . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'

    where constructor = showString . showConstr . toConstr $ t
          slots = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
          commaSlots = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
          listSlots = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
          isTuple = all (==',') (filter (not . flip elem "()") (constructor ""))
          isNull = null (filter (not . flip elem "[]") (constructor ""))
          isList = constructor "" == "(:)"


data Foo = Foo Char Int deriving (Data,Typeable)
