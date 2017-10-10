calcular::[(String,String)] -> Int
calcular [] = 0
calcular ((xv,xp):xs) = if valor_carta xv == 1 && calcular xs <=10 then 11 + calcular xs
			else valor_carta xv + calcular xs

valor_carta:: String -> Int
valor_carta x = 
	if x == "J" then 10
	else if x == "Q" then 10
	else if x == "K" then 10
	else if x == "A" then 1
	else (read x :: Int) 
_____________________________________
calcular::[(String,String)] -> Int
calcular [] = 0
calcular ((xv,xp):xs) =  if valor_carta xv == 1 then hayAces (valor_carta xv + calcular xs)
						 else valor_carta xv + calcular xs

valor_carta:: String -> Int
valor_carta x = 
	if x == "J" then 10
	else if x == "Q" then 10
	else if x == "K" then 10
	else if x == "A" then 1
	else (read x :: Int) 
	
hayAces:: Int -> Int
hayAces x = if x <= 11 then x + 10
			else if x>11 && x<21 then x 
			else if x>21 then x - 10
			else x+ 0 			
