areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo x y z = 
    sqrt(((x + y + z) / 2)*
    (((x + y + z) / 2)-x)*
    (((x + y + z) / 2)-y)*
    (((x + y + z) / 2)-z)) 

comparador :: Int -> Int -> Int 
comparador x y =
    if x < y 
        then -1
    else if x > y
        then 1
        else 0

distanciaPuntos ::(Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

esBisiesto :: Int -> Bool 
esBisiesto x =
    if x `mod` 4 == 0
        then if x `mod` 100 == 0
            then x `mod` 400 /= 0
            else True
    else False

esDecendente :: Int -> Int -> Int -> Int -> Bool
esDecendente x y z w =
    if x > y
        then if y > z 
            then if z > w
                then True
                else False
        else False
    else False

hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt(b^2 + h^2)

maximo :: Int -> Int -> Int -> Int
maximo x y z =
    if x > y
        then if x > z
            then x
            else z
        else if y > z
            then y
            else z

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1) / (x2 - x1)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c =
     (((-b + sqrt(b^2 - 4*a*c)) / (2*a)), 
     (-b - sqrt(b^2 - 4*a*c)) / (2*a))
    