buscar :: [[Char]] -> Int
buscar [[]] = 0
buscar x:xs = if x !! 0 == 'x' then 1
 else buscar xs
