--De deciaml a binario

decBin :: Int -> Int
decBin a = if a == 0 then 0
else if a == 1 then 1
else if div a 2 == 1 && mod a 2 == 0 then 10
else if div a 2 == 1 && mod a 2 == 1 then 11
else (mod a 2) + 10 * decBin(div a 2) 

--De binario a decimal

binDec : :Int -> Int
dinDec n = if n < 2 then n
else mod (n 10) + binDec(div (n 10)) * 2


