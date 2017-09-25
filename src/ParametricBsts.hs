module ParametricBsts where

import Data.Maybe

data BST a =
    Leaf
  | Node { left :: BST a, key :: String, value :: a, right :: BST a}
    deriving (Eq, Show)


bst_search :: String -> BST a -> Maybe a
bst_search k t =
  case t of
    Leaf -> Nothing
    Node { }
      | k < key t -> bst_search k (left t)
      | k > key t -> bst_search k (right t)
      | k == key t -> Just (value t)


et :: BST a -- all values have signatures!
et = Leaf   -- empty tree

t1 :: BST Integer -- there is more than one kind of number
t1 = Node (Node Leaf "Guyer" 5 Leaf) "Hescott" 7 (Node Leaf "Ramsey" 8 Leaf)

data Title = Lec | Ast | Asc  -- definition by choice
             deriving (Show, Eq) -- enable built-in equality and printing

t2 :: BST Title
t2 = Node Leaf "Guyer" Ast (Node Leaf "Hescott" Lec (Node Leaf "Ramsey" Asc Leaf))

search'_tests :: [Bool] -- list of properties
search'_tests =
  [ isNothing (bst_search "Guyer" et)
  , bst_search "Guyer" t1 == Just 5
  , bst_search "Guyer" t2 == Just Ast
  , isNothing (bst_search "Ramsey" et)
  , bst_search "Ramsey" t1 == Just 8
  , bst_search "Ramsey" t2 == Just Asc
  , isNothing (bst_search "Hescott" et)
  , bst_search "Hescott" t1 == Just 7
  , bst_search "Hescott" t2 == Just Lec
  ]
  
