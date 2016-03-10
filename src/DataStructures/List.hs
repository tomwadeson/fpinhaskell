module DataStructures.List where

import Prelude hiding (tail)

data List a = Cons a (List a)
            | Nil
            deriving (Eq, Show)

fromList :: [a] -> List a
fromList = foldr Cons Nil

-- Ex 3.2
tail :: List a -> List a
tail Nil         = undefined
tail (Cons _ xs) = xs
