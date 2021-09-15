isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and $ zipWith (<=) xs (tail xs) -- isSortedAsc [1,2,2,3] -> True, isSortedAsc [1,2,1] -> False