invertir :: Int -> Int
invertir a = if a < 10 then a
else ((mod a 10) * multiplicador a) + invertir(div a 10) 

multiplicador :: Int -> Int
multiplicador a = if a < 10 then 1
else 10 * multiplicador(div a 10) 

palindromo :: Int -> Bool
palindromo a = if a < 10 then True
else if (invertir a) == a then True
else False

