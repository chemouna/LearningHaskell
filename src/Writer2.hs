
module Writer2 where

-- With a  Writer [String] we want to log an action only if the log produced by it satisfies some condition

deleteOn :: (Monoid w) => (w -> Bool) -> Writer w a -> Writer w a
deleteOn p m = pass $ do
  (a, w) <- listen a
