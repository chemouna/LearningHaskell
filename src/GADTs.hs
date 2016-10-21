{-# LANGUAGE GADTs #-}
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

-- let's use phantom types to improve this
data Expr2 a = I2 Int
             | B2 Bool
             | Add2 (Expr2 a) (Expr2 a)
             | Mul2 (Expr2 a) (Expr2 a)
             | Eq2 (Expr2 a) (Expr2 a)
add :: Expr2 Int -> Expr2 Int -> Expr2 Int
add = Add2

i :: Int -> Expr2 Int
i = I2
b :: Bool -> Expr2 Bool
b = B2

mul :: Expr2 Int -> Expr2 Int -> Expr2 Int
mul = Mul2

eval5 :: Expr2 a -> a
-- eval5 (I2 n) = n -- this doesn't type check -> the compiler can't know that I2 mean a = Int
eval5 _ = undefined


-- let's try with GADTs
data Expr3 a where
  I3 :: Int -> Expr3 Int
  B3 :: Bool -> Expr3 Bool
  Add3 :: Expr3 Int -> Expr3 Int -> Expr3 Int
  Mul3 :: Expr3 Int -> Expr3 Int -> Expr3 Int
  Eq3 :: Expr3 Int -> Expr3 Int -> Expr3 Bool

eval6 :: Expr3 a -> a
eval6 (I3 n) = n
eval6 (B3 b) = b
eval6 (Add3 e1 e2) = eval6 e1 + eval6 e2
eval6 (Mul3 e1 e2) = eval6 e1 * eval6 e2
eval6 (Eq3 e1 e2) = eval6 e1 == eval6 e2
