palindromo :: Int -> Bool
palindromo x = calculo x == division x

calculo :: Int -> Int
calculo x = if x <= 9 then x else mod x 10
calculo x = if x >= 9 then calculo (div x 10) else x

division :: Int -> Int
division x = if x >= 9 then division (div x 10) else x
division x = if x <= 9 then x else 0 
