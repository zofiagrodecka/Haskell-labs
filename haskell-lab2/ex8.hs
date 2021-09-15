isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n-1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n | n < 0     = False
         | n == 0    = True
         | otherwise = isOdd (n-1)

--Dla n=1000000000 nie doczekałam się wyniku
--Złożoność czasowa i pamięciowa obu funkcji to O(n)

ackerFun m n
 | m == 0    = n + 1
 | n == 0    = ackerFun (m - 1) 1
 | otherwise = ackerFun (m - 1) (ackerFun m (n - 1))

-- Dla m=0 zawsze można obliczyć ackerfun
-- Dla m = 1 stos się przepełnił przy n= 14940819 (max wartosc n= 14940818)
-- wykres z1(y) = ackerFun(3, y) = 2^(n+3)-3 jest wykładniczy
-- wykres z2(x) = ackerFun(x, 3) nie wiem