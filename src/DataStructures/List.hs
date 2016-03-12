module DataStructures.List where

import Prelude hiding ( tail
                      , head
                      , drop
                      , dropWhile
                      , init
                      , sum
                      , product
                      , length
                      , reverse
                      , concat
                      , map
                      , filter )

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

product :: (Num a) => List a -> a
product Nil         = 1
product (Cons x xs) = x * product xs

-- Ex 3.7
-- Ex 3.8

-- Ex 3.9
length :: List a -> Int
length = foldRight (\_ acc -> acc+1) 0

-- Ex 3.10
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ acc Nil         = acc
foldLeft f acc (Cons x xs) = foldLeft f (f acc x) xs

-- Ex 3.11
sum' :: (Num a) => List a -> a
sum' = foldLeft (+) 0

product' :: (Num a) => List a -> a
product' = foldLeft (*) 1

length' :: List a -> Int
length' = foldLeft (\acc _ -> acc+1) 0

-- Ex 3.12
reverse :: List a -> List a
reverse = foldLeft (\acc x -> Cons x acc) Nil -- (flip Cons) works too

-- Ex 3.13
foldRight' :: (a -> b -> b) -> b -> List a -> b
foldRight' f acc xs = foldLeft (flip f) acc (reverse xs)

-- Ex 3.14
append :: List a -> List a -> List a
append = foldRight Cons

-- Ex 3.15
concat :: List (List a) -> List a
concat = foldRight (flip append) Nil

-- Ex 3.16
addOneToAll :: (Num a) => List a -> List a
addOneToAll = foldRight (\x acc -> Cons (x+1) acc) Nil

-- Ex 3.17
dtos :: List Double -> List String
dtos = foldRight (\x acc -> Cons (show x) acc) Nil

-- Ex 3.18
map :: (a -> b) -> List a -> List b
map f = foldRight (\x acc -> Cons (f x) acc) Nil

-- Ex 3.19
filter :: (a -> Bool) -> List a -> List a
filter p = foldRight (\x acc -> if p x then Cons x acc else acc) Nil

-- 3.20
flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat . map f
