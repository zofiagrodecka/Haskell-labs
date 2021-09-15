sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr f acc xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 0

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl f (f x acc) xs

sumWith''' g  = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 0