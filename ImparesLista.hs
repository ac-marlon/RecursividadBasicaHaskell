buscar::[Int] -> [Int] 
buscar [] = [] 
buscar (x:(_:xs)) = (x:(buscar xs))                                                
