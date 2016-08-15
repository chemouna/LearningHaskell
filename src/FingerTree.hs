
module FingerTree where

data FingerTree = Empty
  | Single a
  | Deep (Digit a) (FingerTree (Node a) (Digit a))

  
