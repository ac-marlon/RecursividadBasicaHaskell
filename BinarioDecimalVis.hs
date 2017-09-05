decBin :: Int -> Int
decBin a = if a == 0 then 0
else if a == 1 then 1
else if div a 2 == 1 && mod a 2 == 0 then 10
else if div a 2 == 1 && mod a 2 == 1 then 11
else (mod a 2) + 10 * decBin(div a 2) 

contador :: Int -> Int
contador 0 = 0
contador a = 1 + contador(div a 10) 

binDec :: Int -> Int
binDec a = if a == 0 then 0 
else if a == 1 then 1
else if mod a 2 == 0 && div a 2 == 5 then 2
else if mod a 2 == 1 && div a 2 == 5 then 3
else (2 ^ ((contador a) * (mod a 10))) + binDec(div a 10)

--------------------------------------------------------------------------------------------------------------------------------

contador :: Int -> Int
contador 0 = 0
contador a = 1 + contador(div a 10) 

binDec :: Int -> Int
binDec a = if a == 0 then 0 
else if a == 1 then 1
else if mod a 2 == 0 && div a 2 == 5 then 2
else if mod a 2 == 1 && div a 2 == 5 then 3
else (2 ^ (((contador (a)))) * (mod a 10))) + binDec(div a 10)

multiplicador :: Int -> Int
multiplicador a = if a < 10 then 1
else 10 * multiplicador(div a 10) 
