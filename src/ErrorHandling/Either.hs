module ErrorHandling.Either where

import Prelude hiding (Either(..))

data Either e a = Left e
                | Right a
                deriving (Eq, Show)

-- Ex 4.6
map :: (a -> b) -> Either e a -> Either e b
map f (Right x) = Right (f x)
map _ (Left x)  = Left x

flatMap :: (a -> Either e b) -> Either e a -> Either e b
flatMap f (Right x) = f x
flatMap _ (Left x)  = Left x

orElse :: Either e a -> Either e a -> Either e a
orElse (Left _) x  = x
orElse (Right x) _ = Right x

map2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
map2 _ (Left x) _          = Left x
map2 _ _ (Left x)          = Left x
map2 f (Right x) (Right y) = Right (f x y)
