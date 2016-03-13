module DataStructures.Tree where

data Tree a = Leaf a
            | Branch { left :: Tree a, right :: Tree a}
            deriving (Eq, Show)

size :: Tree a -> Int
size (Leaf _)     = 1
size (Branch l r) = 1 + size l + size r
