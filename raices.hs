raices :: Float -> Float -> Float -> (Float, Float)
raices a b c =
     (((-b + sqrt(b^2 - 4*a*c)) / (2*a)), 
     (-b - sqrt(b^2 - 4*a*c)) / (2*a))
    