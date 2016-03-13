module ErrorHandling.Option where

import Prelude hiding (map)
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
