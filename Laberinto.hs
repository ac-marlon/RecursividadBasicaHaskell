buscar::[[String]] -> Int
buscar [[]] = 0
buscar (x:xs)=  if x !! 0 == "x" then 0
                if else x !! 1 == "x" then 1
                if else x !! 2 == "x" then 2
                if else x !! 3 == "x" then 3
                if else x !! 4 == "x" then 4
		              else buscar xs
