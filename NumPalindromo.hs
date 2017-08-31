palindromo :: Int -> Bool
palindromo x = 
 		((div x 1000) == (mod x 10)) && 
 		((mod (div x 100) 10) == (div (mod x 100) 10))
