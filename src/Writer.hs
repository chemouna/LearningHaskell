{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}

module Writer where

import Control.Monad.Writer

fact1 :: Integer -> Writer String Integer
fact1 0 = return 1
fact1 n = do
  let n' = n-1
  tell $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact1 n'
  tell $ "We've called f " ++ show m ++ "\n"
  let r = n*m
  tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  return r

-- with a writer that just counts the number of times we do substraction & multiplication
fact2 :: Integer -> Writer (Sum Integer) Integer
fact2 0 = return 1
fact2 n = do
  let n' = n-1
  tell $ Sum 1
  m <- fact2 n'
  let r = n*m
  tell $ Sum 1
  return r

-- with a flag that is switched on when we get to some value then switched off
fact4 :: Integer -> Writer Any Integer
fact4 0 = return 1
fact4 n = do
  let n' = n-1
  m <- fact4 n'
  let r = n*m
  tell $ Any (r==120)
  return r

-- using dual to collect trace in reverse order
fact5 :: Integer -> Writer (Dual String) Integer
fact5 0 = return 1
fact5 n = do
  let n'=n-1
  tell $ Dual $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact5 n'
  tell $ Dual $ "We've called f " ++ show m ++ "\n"
  let r = n*m
  tell $ Dual $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  return r

-- using the product monoid to accumulate two side effects at the same time

-- tellFst :: (Monoid t1, MonadWriter (t, t1) m) => t -> m ()
tellFst a = tell $ (a,mempty)

-- tellSnd :: (Monoid t1, MonadWriter (t, t1) m) => t -> m ()
tellSnd b = tell $ (mempty,b)

fact6 :: Integer -> Writer (String, Sum Integer) Integer
fact6 0 = return 1
fact6 n = do
  let n' = n-1
  tellSnd $ Sum 1
  tellFst $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact6 n'
  tellFst $ "We've called f " ++ show m ++ "\n"
  let r = n*m
  tellSnd $ Sum 1
  tellFst $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  return r
