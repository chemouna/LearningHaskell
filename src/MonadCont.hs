module MonadCont where

import Control.Monad.Cont

-- ex 1 
calculateLength :: [a] -> Cont r Int
calculateLength l = return (length l)

lengthPrint = do
  runCont (calculateLength "12345") print

double :: Int -> Cont r Int
double n = return (n * 2)

doubleLengthPring = do
  runCont (calculateLength "12345" >>= double) print

-- ex 2
validateName name exit = do
  when (null name) (exit "You didn't tell me your name!")

whatsYourName :: String -> String
whatsYourName name =
  (`runCont` id) $ do
    response <- callCC $ \exit -> do
      validateName name exit
      return $ "Welcome, "++ name ++ "!"
    return response
