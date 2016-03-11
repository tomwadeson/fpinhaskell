module DataStructures.List where

import Prelude hiding (tail, head, drop, dropWhile, init)

data List a = Cons a (List a)
            | Nil
            deriving (Eq, Show)

fromList :: [a] -> List a
fromList = foldr Cons Nil

head :: List a -> a
head Nil        = undefined
head (Cons x _) = x

-- Ex 3.2
tail :: List a -> List a
tail Nil         = undefined
tail (Cons _ xs) = xs

-- Ex 3.3
setHead :: List a -> a -> List a
setHead Nil _         = undefined
setHead (Cons _ xs) x = Cons x xs

-- Ex 3.4
drop :: List a -> Int -> List a
drop Nil _         = Nil
drop xs 0          = xs
drop (Cons x xs) n = drop xs (n-1)

-- Ex 3.5
dropWhile :: List a -> (a -> Bool) -> List a
dropWhile Nil _ = Nil
dropWhile list@(Cons x xs) p
  | p x       = dropWhile xs p
  | otherwise = list 

-- Ex 3.6
init :: List a -> List a
init Nil          = undefined
init (Cons x Nil) = Nil
init (Cons x xs)  = Cons x (init xs)
