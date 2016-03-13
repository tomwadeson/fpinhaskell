module DataStructures.Tree where

import Prelude hiding (maximum, map)

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

-- Ex 3.27
depth :: Tree a -> Int
depth (Leaf _)     = 0
depth (Branch l r) = 1 + max (depth l) (depth r) 

-- Ex 3.28
map :: (a -> b) -> Tree a -> Tree b
map f (Leaf x)     = Leaf (f x)
map f (Branch l r) = Branch (map f l) (map f r)

-- Ex 3.29
fold :: (a -> b) -> (b -> b -> b) -> Tree a -> b
fold f _ (Leaf x)     = f x
fold f g (Branch l r) = g (fold f g l) (fold f g r)

size' :: Tree a -> Int
size' = fold (const 1) (\x y -> 1 + x + y)

maximum' :: Tree Int -> Int
maximum' = fold id max

depth' :: Tree a -> Int
depth' = fold (const 0) (\x y -> 1 + max x y)

map' :: (a -> b) -> Tree a -> Tree b
map' f = fold (Leaf . f) Branch
