module DataStructures.Tree where

import Prelude hiding (maximum)

data Tree a = Leaf a
            | Branch { left :: Tree a, right :: Tree a }
            deriving (Eq, Show)

-- Ex 3.25
size :: Tree a -> Int
size (Leaf _)     = 1
size (Branch l r) = 1 + size l + size r

-- Ex 3.26
maximum :: Tree Int -> Int
maximum (Leaf x)     = x
maximum (Branch l r) = max (maximum l) (maximum r)
