module GADTs where

import Data.Maybe

data Expr = I Int
  | B Bool
  | Add Expr Expr
  | Mul Expr Expr
  | Eq Expr Expr

eval :: Expr -> Int
eval (I n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

eval2 :: Expr -> Either Int Bool
eval2 (I n) = Left n
eval2 (B b) = Right b
-- eval2 (Add e1 e2) = eval2 e1 + eval2 e2 -- doesn't type check

eval3 :: Expr -> Maybe (Either Int Bool)
eval3 (I n) = return $ Left n
eval3 (B b) = return $ Right b
eval3 (Add e1 e2) = do
  a1 <- evalIntExpr e1
  a2 <- evalIntExpr e2
  return $ Left $ a1 +  a2
eval3 (Mul e1 e2) = do
  a1 <- evalIntExpr e1
  a2 <- evalIntExpr e2
  return $ Left $ a1 * a2
eval3 (Eq e1 e2) = do
  a1 <- evalIntExpr e1
  a2 <- evalIntExpr e2
  return $ Right $ a1 == a2

evalIntExpr :: Expr -> Maybe Int
evalIntExpr e = eval3 e >>= unwrap
  where unwrap (Left x) = Just x
        unwrap (Right x) = Nothing

evalBoolExpr :: Expr -> Maybe Bool
evalBoolExpr e = eval3 e >>= unwrap
  where unwrap (Left x) = Nothing
        unwrap (Right x) = Just x

-- let's try to remove the duplication of code we have 
eval4 :: Expr -> Maybe (Either Int Bool)
eval4 (I n) = return $ Left n
eval4 (B b) = return $ Right b
eval4 (Add e1 e2) = applyIntOp Left (+) e1 e2
eval4 (Mul e1 e2) = applyIntOp Left (*) e1 e2

applyIntOp :: (t -> b) -> (Int -> Int -> t) -> Expr -> Expr -> Maybe b
applyIntOp c f e1 e2 = do
  a1 <- evalIntExpr4 e1
  a2 <- evalIntExpr4 e2
  return $ c $ f a1 a2

evalIntExpr4 :: Expr -> Maybe Int
evalIntExpr4 e = eval4 e >>= (either Just (\_ -> Nothing))

evalBoolExpr4 :: Expr -> Maybe Bool
evalBoolExpr4 e = eval4 e >>= (either (\_ -> Nothing) Just)

-- let's see if we can improve it with applicative 

