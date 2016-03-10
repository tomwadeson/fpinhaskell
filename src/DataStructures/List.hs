module DataStructures.List where

import Prelude hiding (tail, head)

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
