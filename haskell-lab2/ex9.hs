qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<= x) xs --[ y | y <- xs, y <= x ]
   rightPart xs = filter (>x) xs  --[ y | y <- xs, y > x  ]


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [a] = [a]
mSort xs = merge (mSort (leftPart xs))  (mSort (rightPart xs))
  where
    leftPart xs = take ((length xs) `div` 2) xs
    rightPart xs = drop ((length xs) `div` 2) xs


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y then (x:y:ys) else (y:(insert x ys))

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)


concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:[]) = x
concat'' (x:xs) = x ++ concat'' xs