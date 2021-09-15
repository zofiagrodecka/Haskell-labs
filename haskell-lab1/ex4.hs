sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x,y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (i, c) = (c, i)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x == y) && (y == z)

areaOfATriangle :: (Double, Double, Double) -> Double
areaOfATriangle (x, y, z) = sqrt((x+y+z)*(x+y-z)*(x-y+z)*(-x+y+z))/4
