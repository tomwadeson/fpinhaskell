module ErrorHandling.Option where

import Prelude hiding (map, sequence, traverse)
import qualified Prelude as P

data Option a = None
              | Some a
              deriving (Eq, Show)

-- Ex 4.1
map :: (a -> b) -> Option a -> Option b
map _ None     = None
map f (Some x) = Some (f x)

flatMap :: (a -> Option b) -> Option a -> Option b
flatMap _ None     = None
flatMap f (Some x) = f x

getOrElse :: Option a -> a -> a
getOrElse None d     = d
getOrElse (Some x) _ = x

orElse :: Option a -> Option a -> Option a
orElse x y = case x of
  Some _ -> x
  None   -> y

filter :: (a -> Bool) -> Option a -> Option a
filter _ None = None
filter p o@(Some x)
  | p x       = o
  | otherwise = None

-- Ex 4.2
variance :: [Double] -> Option Double
variance xs = flatMap variance' $ mean xs
  where
    variance' m = mean . P.map (\x -> (x-m)^2) $ xs

mean :: [Double] -> Option Double
mean [] = None
mean xs = Some (sum xs / (fromIntegral . length $ xs))

-- Ex 4.3
map2 :: (a -> b -> c) -> Option a -> Option b -> Option c
map2 _ _ None            = None
map2 _ None _            = None
map2 f (Some x) (Some y) = Some (f x y)

-- Ex 4.4
sequence :: [Option a] -> Option [a]
sequence = foldr (map2 (:)) (Some [])

-- Ex 4.5
traverse :: (a -> Option b) -> [a] -> Option [b]
traverse f = foldr (map2 (:) . f) (Some [])

sequence' :: [Option a] -> Option [a]
sequence' = traverse id
