
module SearchAlgorithms where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

traverseDfs :: Tree a -> [a]
traverseDfs Empty = []
traverseDfs (Node a l r) = [a] ++ traverseDfs l ++ traverseDfs r

traverseBfs :: Tree a -> [a]
traverseBfs Empty = []
traverseBfs t = tbf [t]
  where
    tbf xs = map nodeValue xs ++ tbf (concatMap leftRightNodes xs)
    nodeValue (Node a _ _) = a
    leftRightNodes (Node _ Empty Empty) = []
    leftRightNodes (Node _ a Empty) = [a]
    leftRightNodes (Node _ Empty b) = [b]
    leftRightNodes (Node _ a b) = [a,b]

createTree = Node 'A'
                (Node 'B'
                    (Node 'C' Empty Empty)
                    (Node 'D' Empty Empty)
                )
                (Node 'E'
                    (Node 'F' Empty Empty)
                    (Node 'G' Empty (Node 'H'
                        (Node 'I' Empty Empty)
                        Empty
                    ))
                )


