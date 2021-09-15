fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

secDivfst :: Integral a => [a] -> Bool
secDivfst (x : y : _) | y `mod` x == 0 = True
secDivfst _                            = False


thirdDivfst :: Integral a => [a] -> Bool
thirdDivfst (x : y : z : _) | z `mod` x == 0 = True
thirdDivfst _                            = False