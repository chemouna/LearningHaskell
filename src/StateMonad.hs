module StateMonad where

import Control.Monad
import Control.Monad.State

-- some State Monad examples

-- Stack

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

swap :: State Stack ()
swap = do a <- pop
          b <- pop
          push a
          push b

dup :: State Stack ()
dup = do a <- pop
         push a
         push a

drop :: State Stack ()
drop = do pop
          a <- pop
          push a

changeStack :: State Stack Int
changeStack = push 3 >> push 2 >> push 3 >>= \_ -> pop


-- Sequence

data Seq = Seq String Int

instance Show Seq where
  show (Seq s i) = s ++ show i

instance Enum Seq where
  succ (Seq s i) = Seq s $ succ i
  pred (Seq s i) = Seq s $ pred i
  toEnum i = Seq "" i
  fromEnum (Seq s i) = i

nextSeq :: (Enum s, MonadState s m) => m s
nextSeq = do s <- get
             put (succ s)
             return s

-- nextSeq = get >>= put succ return

next :: (Show s, Enum s, MonadState s m) => String -> m String
next pre = nextSeq >>= \s -> return (pre ++ show s)


-- Fizzbuzz

