module DataStructures.List where

import Prelude hiding (tail, head, drop, dropWhile, init, sum, product)

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
drop (Cons _ xs) n = drop xs (n-1)

-- Ex 3.5
dropWhile :: List a -> (a -> Bool) -> List a
dropWhile Nil _ = Nil
dropWhile list@(Cons x xs) p
  | p x       = dropWhile xs p
  | otherwise = list 

-- Ex 3.6
init :: List a -> List a
init Nil          = undefined
init (Cons _ Nil) = Nil
init (Cons x xs)  = Cons x (init xs)

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ acc Nil         = acc
foldRight f acc (Cons x xs) = f x (foldRight f acc xs)

sum :: (Num a) => List a -> a
sum Nil         = 0
sum (Cons x xs) = x + sum xs

sum' :: (Num a) => List a -> a
sum' = foldRight (+) 0

product :: (Num a) => List a -> a
product Nil         = 1
product (Cons x xs) = x * product xs

product' :: (Num a) => List a -> a
product' = foldRight (*) 1
