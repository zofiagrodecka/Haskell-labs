import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

--pointfree version
--ghci> let sortDesc2 = reverse . sort

-- Zad 3.
--let w3 = \x y z -> sqrt (x^2 + y^2 + z^2)
--ghci> (f . (w3 1 3) . h) 3


