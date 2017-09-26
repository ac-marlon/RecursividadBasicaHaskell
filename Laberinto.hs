laberinto :: [[Char]]
laberinto = [ 
	   ['1','0','F','1','1','1','1'],
	   ['1','0','1','1','1','1','1'],
	   ['1','0','0','1','1','1','1'],
	   ['1','1','X','1','1','1','1'],
	   ['1','1','1','1','1','1','1']]


-- Muestra el elemento en la posicion (fila,columna)
elemento :: [[Char]]->(Int,Int)->Char
elemento m (fil,col) =  (m!!fil)!!col

