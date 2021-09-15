funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x]

funcListExt :: [ Double -> Double ]
funcListExt = funcList ++[\x -> sqrt(x+1)]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)