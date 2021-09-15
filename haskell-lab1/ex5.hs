sgn :: Int -> Int
sgn n = if n < 0
       then -1
       else if n == 0
            then 0
            else 1

absInt :: Int -> Int -- absInt 2 = absInt (-2) = 2
absInt x = if x>0
  then x
  else (-x)

min2Int :: (Int, Int) -> Int -- min (1,2) = 1, min (-1, -1) = -1
min2Int (x, y) = if x < y
  then x
  else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = if x < y
  then if x < z
    then x
    else z
  else if y < z
    then y
    else z

min3Int_v2 :: (Int, Int, Int) -> Int
min3Int_v2 (a, b, c) = min2Int(min2Int(a, b), min2Int(b,c))

toUpper :: Char -> Char
toUpper c = if fromEnum(c) <= 122 && fromEnum(c) >= 97
  then toEnum(fromEnum(c)-32)
  else c

toLower :: Char -> Char
toLower c = if fromEnum(c) <= 90 && fromEnum(c) >= 65
  then toEnum(fromEnum(c)+32)
  else c

isDigit :: Char -> Bool
isDigit c = if fromEnum(c) >= 48 && fromEnum(c) <= 57
  then True
  else False

charToNum :: Char -> Int
charToNum c = fromEnum(c)

romanDigit :: Char -> String
romanDigit c = if fromEnum(c) == 49
  then "I"
  else if fromEnum(c) == 50
    then "II"
  else if fromEnum(c) == 51
    then "III"
  else if fromEnum(c) == 52
    then "IV"
  else if fromEnum(c) == 53
    then "V"
  else if fromEnum(c) == 54
    then "VI"
  else if fromEnum(c) == 55
    then "VII"
  else if fromEnum(c) == 56
    then "VIII"
  else if fromEnum(c) == 57
    then "IX"
  else "Nie podales cyfry"
