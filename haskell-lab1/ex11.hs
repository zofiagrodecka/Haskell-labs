roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where {d = sqrt (b * b - 4 * a * c);
       e = 2 * a;} -- uwaga na przesunięcie!

-- wersja z let .. in :
roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
    let {d = sqrt (b*b - 4*a*c);
        e = 2*a;}
    in ( (-b - d) / e, (-b + d) / e)
