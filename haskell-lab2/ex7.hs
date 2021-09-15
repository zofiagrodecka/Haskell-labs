sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
  where loop acc []     = acc
        loop acc (x:xs) = loop (x + acc) xs


prod'2 :: Num a => [a] -> a
prod'2 = loop 1
  where loop acc [] = 0
        loop acc (x:xs) = loop (x * acc) xs


length'2 :: [a] -> Int
length'2 x = f 0 x
  where f acc [] = acc
        f acc (x:xs) = f (1 + acc) xs