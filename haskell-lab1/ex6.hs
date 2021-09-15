absInt :: Int -> Int
absInt n | n > 0 = n
         | otherwise = -n

sgn :: Int -> Int
sgn x | x < 0 = -1
      | x > 0 = 1
      | otherwise = 0

min3Int :: (Int, Int, Int) -> Int -- min (1,2,3)=1, min (1,1,3)=1
min3Int (x, y, z) | x < y && x < z = x
                  | x < y && x >= z = z
                  | x >= y && y < z = y
                  | x >= y && y >= z = z

toUpper :: Char -> Char
toUpper c | fromEnum(c) <= 122 && fromEnum(c) >= 97 = toEnum(fromEnum(c)-32)
          | otherwise = c

toLower :: Char -> Char
toLower c | fromEnum(c) <= 90 && fromEnum(c) >= 65 = toEnum(fromEnum(c)+32)
          | otherwise = c

isDigit :: Char -> Bool
isDigit c |fromEnum(c) >= 48 && fromEnum(c) <= 57 = True
          | otherwise = False

charToNum :: Char -> Int
charToNum c = fromEnum(c)

romanDigit :: Char -> String
romanDigit c | fromEnum(c) == 49 = "I"
             | fromEnum(c) == 50 = "II"
             | fromEnum(c) == 51 = "III"
             | fromEnum(c) == 52 = "IV"
             | fromEnum(c) == 53 = "V"
             | fromEnum(c) == 54 = "VI"
             | fromEnum(c) == 55 = "VII"
             | fromEnum(c) == 56 = "VIII"
             | fromEnum(c) == 57 = "IX"
             | otherwise = "Nie podales cyfry"
