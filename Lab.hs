matriz :: [[Char]]
matriz = [ 
	   ['1','1','1','1','1','1','1','1'],
	   ['1','1','1','1','1','1','1','1'],
	   ['1','F','0','0','1','1','1','1'],
	   ['1','1','X','0','1','1','1','1'],
	   ['1','1','1','1','1','1','1','1'],
	   ['1','1','1','1','1','1','1','1']]


-- Muestra el elemento en la posicion (fila,columna)
indicex :: [[Char]]->(Int,Int)->Char
indicex y (fila,columna) =  (y!!fila)!!columna


-- Mover la coordenada a la izquierda
moverIzquierda :: (Int,Int)->(Int,Int)
moverIzquierda (a,b) = (a,b-1)


-- Mover la coordenada a la derecha
moverDerecha :: (Int,Int)->(Int,Int)
moverDerecha (a,b) = (a,b+1)

-- Mover la coordenada a la derecha
moverArriba :: (Int,Int)->(Int,Int)
moverArriba (a,b) = (a-1,b)

-- Mover la coordenada a la izquierda
moverAbajo :: (Int,Int)->(Int,Int)
moverAbajo (a,b) = (a+1,b)


--Halla la posicion inicial del laberinto
posicionInicial :: [[Char]]->(Int,Int)
posicionInicial x = moverVertical x 'X' 0
 
--Halla la posicion final del laberinto
posicionFinal :: [[Char]]->(Int,Int)
posicionFinal x = moverVertical x 'F' 0
