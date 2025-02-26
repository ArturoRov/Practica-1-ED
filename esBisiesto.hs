esBisiesto :: Int -> Bool 
esBisiesto x =
    if x `mod` 4 == 0
        then if x `mod` 100 == 0
            then x `mod` 400 /= 0
            else True
    else False