sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

{-
Zad 3.
ghci> let sum = sumWith (\x -> x)
ghci> let sumSqr = sumWith (\x -> x^2)
ghci> let sumCube = sumWith (\x -> x^3)
ghci> let sumAbs = sumWith (\x -> abs x)

Zad 4.
ghci> sumWith (\x -> x^5) [1..15]

Zad 5.
ghci> let listLength = sumWith (\x -> (1))
-}
