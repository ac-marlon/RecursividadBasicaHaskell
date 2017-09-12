decBin :: Int -> Int
decBin a = if a == 0 then 0
else if a == 1 then 1
else if div a 2 == 1 && mod a 2 == 0 then 10
else if div a 2 == 1 && mod a 2 == 1 then 11
else (mod a 2) + 10 * decBin(div a 2) 


binDec :: Int -> Int
binDec a = if a < 2 then a
else (mod a 10) + binDec(div a 10) * 2
