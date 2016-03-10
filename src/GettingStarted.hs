module GettingStarted where

import Prelude hiding (curry, uncurry)

-- Ex 2.1
fib :: Integer -> Integer
fib = fib' 0 1
  where
    fib' x _ 0 = x
    fib' x y n = fib' y (x+y) (n-1)

-- Ex 2.2
isSorted :: (Ord a) => [a] -> (a -> a -> Bool) -> Bool
isSorted [] _       = True
isSorted [_] _      = True
isSorted (x:y:xs) f = f x y && isSorted xs f 

-- Ex 2.3
curry :: ((a, b) -> c) -> a -> b -> c
curry f x = f . (,) x

-- Ex 2.4
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- Ex 2.5
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \x -> f (g x)
