module ContravariantFunctor where

import Data.Char (toUpper)
import Data.Functor.Contravariant
import Data.Set
import Data.Set (toList)
import Data.Either

lengther :: Op Int [a]
lengther = Op Prelude.length

setLengther :: Op Int (Set a)
setLengther = contramap toList lengther

data Behavior a = Behavior (a -> IO (Behavior a))

instance Contravariant Behavior where
  contramap f (Behavior r) = Behavior (\a -> fmap (contramap f) (r (f a)))

runBehavior :: Behavior a -> [a] -> IO ()
runBehavior _ [] = return ()
runBehavior (Behavior f) (a:as) = do
  newBehavior <- f a
  runBehavior newBehavior as

printer :: Behavior String
printer = Behavior (\s -> putStrLn s >> return printer)

messages :: [String]
messages = ["Hello", "World", "From", "The", "Awesome", "Haskell"]

testPrinter = runBehavior printer messages

-- with contramap let's make it shout
shouter :: Behavior String
shouter = contramap (\s -> (Prelude.map toUpper s ++ "!")) printer

testShouter = runBehavior shouter messages

makeMailbox :: [(String, String)] -> Behavior (String, String)
makeMailbox messages = Behavior $ \message ->
  if length messages < 3
  then let newMessages = messages ++ [message]
       in do putStrLn "I contain messages:"
             mapM_ printMessage newMessages
             return (makeMailbox newMessages)
  else do putStrLn "I am full."
          putStrLn "I contain messages:"
          mapM_ printMessage messages
          return (makeMailbox messages)
  where
    printMessage (from, message) =
      putStrLn ("From " ++ from ++ ": " ++ message)

mailbox :: Behavior (String, String)
mailbox = makeMailbox []

testMailbox = runBehavior mailbox messages
  where messages = [ ("ocharles", "hackage is great")
                   , ("edwardk", "I love Simon Peyton Jones")
                   , ("spj", "We all love lazy evaluation")
                   , ("Your spouse", "Make me a cup of tea")
                   , ("Your employer", "You must program in Scala") ]

-- lets use contramap to make mailbox our logger
logMsg :: Either String Int -> (String, String)
logMsg (Right x) =  ("Logger daemon", "Everything is OK: " ++ show x)
logMsg (Left s) = ("Logger daemon", "FAILURE: " ++ s)

loggerMailbox :: Behavior (Either String Int)
loggerMailbox = contramap logMsg mailbox

testLogger = runBehavior loggerMailbox messages
  where messages = [ Right 24
                   , Left "Oops, there was an error"
                   , Right 1
                   , Right 2 ]
