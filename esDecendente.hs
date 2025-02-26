esDecendente :: Int -> Int -> Int -> Int -> Bool
esDecendente x y z w =
    if x > y
        then if y > z 
            then if z > w
                then True
                else False
        else False
    else False
