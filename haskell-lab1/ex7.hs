not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True -- :)
isItTheAnswer _      = False

or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' (_, _) = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (_, _) = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True) = False
nand' (_, _) = True

xor' :: (Bool, Bool) -> Bool
xor' (True, True) = False
xor' (False, False) = False
xor' (_, _) = True
