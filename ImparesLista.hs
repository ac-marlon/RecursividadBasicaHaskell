buscar::[Int] -> [Int] 
buscar [] = []
buscar (x:xs) = x : (buscar xs)
