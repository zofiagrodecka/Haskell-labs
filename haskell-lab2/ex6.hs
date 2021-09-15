fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 0
prod' [x] = x
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' (True : xs) = True
or' (False : xs) = or' xs

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = True
and' (True : xs) = and' xs
and' (False : xs) = False

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' a [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = x * 2 : doubleAll xs

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = x^2 : squareAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven x = if (head x) `mod` 2 == 0 then [(head x)] ++ selectEven (tail x) else selectEven (tail x)