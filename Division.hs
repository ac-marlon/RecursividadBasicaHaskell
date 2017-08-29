dividir::Int->Int->Int
dividir a 0 = 0
dividir 0 b = 0
dividir a b = if b > a then 0 else dividir (a-b) b +1
