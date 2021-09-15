not' :: Bool -> Bool
not' b = case b of
          True  -> False
          False -> True

absInt n =
 case (n >= 0) of
   True -> n
   _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer x = case x of
                  "Love" -> True -- love is always the answer ♥
                  _ -> False

or' :: (Bool, Bool) -> Bool
or' (x, y) = case (x, y) of
                (False, False) -> False
                (_, _) -> True

and' :: (Bool, Bool) -> Bool
and' (x, y) = case (x, y) of
                (True, True) -> True
                (_, _) -> False

nand' :: (Bool, Bool) -> Bool
nand' (x, y) = case (x, y) of
                  (True, True) -> False
                  (_, _) -> True

xor' :: (Bool, Bool) -> Bool
xor' (x, y) = case (x, y) of
                (True, True) -> False
                (False, False) -> False
                (_, _) -> True
