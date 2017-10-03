matriz :: [[String]]
matriz = [ 
	   ["1","1","x","1","1","1","1"],
	   ["1","1","0","1","1","1","1"],
	   ["1","1","0","1","1","1","1"],
	   ["1","1","0","0","0","f","1"],
	   ["1","1","1","1","1","1","1"]]

-- Muestra las coordenadas de la solución    
showRoad::[[String]]->[(Int,Int)]
showRoad a = recorridoLeft a (encontrarX a (0,0)) 	   
	   
-- Encuentra X
encontrarX:: [[String]]->(Int,Int)->(Int,Int)
encontrarX a (row,col)
    | indiceXY a (row,col) == "x" = (row,col)
    | indiceXY a (row,col) /= "x" = encontrarX a (move a (row,col))  	   
	   
-- Retorna las coordenadas 
indiceXY :: [[String]]->(Int,Int)->String
indiceXY a (row,col) =  (a!!row)!!col

-- Se mueve de izquierda a derecha y de arriba hacia abajo 
move:: [[String]]->(Int,Int)->(Int,Int)
move (a:as) (row,col) = if (col< ((length a)-1)) 
                                then (row,col+1) 
                            else if (col== ((length a)-1)) 
                                then (row+1,0) 
                            else (row,col)

-- Mover hacia la izquierda   
moveLeft::[[String]]->(Int,Int)->(Int,Int)
moveLeft (a:as) (row,col)
    | col== 0 = (row,col)
    | col> 0 = (row,col-1)       
             
-- Mover hacia la derecha
moveRight::[[String]]->(Int,Int)->(Int,Int)
moveRight (a:as) (row,col)
    | col== ((length a)-1) = (row,col)
    | col< ((length a)-1) = (row,col+1)

-- Mover hacia arriba
moveUp::[[String]]->(Int,Int)->(Int,Int)
moveUp (a:as) (row,col)
    | row == 0 = (row,col)
    | row > 0 = (row-1,col)


-- Mover hacia abajo
moveDown::[[String]]->(Int,Int)->(Int,Int)
moveDown (a:as) (row,col)
    | row == ((length (a:as))-1) = (row,col)
    | row < ((length (a:as))-1) = (row+1,col)

-- Secuencia para comprobar posiciones a la izquierda
recorridoLeft::[[String]]->(Int,Int)->[(Int,Int)]
recorridoLeft a (row,col)
    |indiceXY a (row,col) == "f" = [(row,col)]
    |indiceXY a (moveLeft a (row,col)) == "0" || indiceXY a (moveLeft a (row,col)) == "f" = [moveLeft a (row,col)] ++ recorridoLeft a (moveLeft a (row,col))
    |indiceXY a (moveUp a (row,col)) == "0" || indiceXY a (moveUp a (row,col)) == "f" = [moveUp a (row,col)] ++ recorridoUp a (moveUp a (row,col))
    |indiceXY a (moveDown a (row,col)) == "0" || indiceXY a (moveDown a (row,col)) == "f" = [moveDown a (row,col)] ++ recorridoDown a (moveDown a (row,col))
    |indiceXY a (moveRight a (row,col)) == "0" || indiceXY a (moveRight a (row,col)) == "f" = [moveRight a (row,col)] ++ recorridoRight a (moveRight a (row,col))
    
-- Secuencia para comprobar posiciones arriba
recorridoUp::[[String]]->(Int,Int)->[(Int,Int)]
recorridoUp a (row,col)
    |indiceXY a (row,col) == "f" = [(row,col)]
    |indiceXY a (moveUp a (row,col)) == "0" || indiceXY a (moveUp a (row,col)) == "f" = [moveUp a (row,col)] ++ recorridoUp a (moveUp a (row,col))
    |indiceXY a (moveRight a (row,col)) == "0" || indiceXY a (moveRight a (row,col)) == "f" = [moveRight a (row,col)] ++ recorridoRight a (moveRight a (row,col))
    |indiceXY a (moveLeft a (row,col)) == "0" || indiceXY a (moveLeft a (row,col)) == "f" = [moveLeft a (row,col)] ++ recorridoLeft a (moveLeft a (row,col))
    |indiceXY a (moveDown a (row,col)) == "0" || indiceXY a (moveDown a (row,col)) == "f" = [moveDown a (row,col)] ++ recorridoDown a (moveDown a (row,col))
    
-- Secuencia para comprobar posiciones a la derecha
recorridoRight::[[String]]->(Int,Int)->[(Int,Int)]
recorridoRight a (row,col)
    |indiceXY a (row,col) == "f" = [(row,col)]
    |indiceXY a (moveRight a (row,col)) == "0" || indiceXY a (moveRight a (row,col)) == "f" = [moveRight a (row,col)] ++ recorridoRight a (moveRight a (row,col))
    |indiceXY a (moveUp a (row,col)) == "0" || indiceXY a (moveUp a (row,col)) == "f" = [moveUp a (row,col)] ++ recorridoUp a (moveUp a (row,col))
    |indiceXY a (moveDown a (row,col)) == "0" || indiceXY a (moveDown a (row,col)) == "f" = [moveDown a (row,col)] ++ recorridoDown a (moveDown a (row,col))
    |indiceXY a (moveLeft a (row,col)) == "0" || indiceXY a (moveLeft a (row,col)) == "f" = [moveLeft a (row,col)] ++ recorridoLeft a (moveLeft a (row,col))
    
    
-- Secuencia para comprobar posiciones abajo
recorridoDown::[[String]]->(Int,Int)->[(Int,Int)]
recorridoDown a (row,col)
    |indiceXY a (row,col) == "f" = [(row,col)]
    |indiceXY a (moveDown a (row,col)) == "0" || indiceXY a (moveDown a (row,col)) == "f" = [moveDown a (row,col)] ++ recorridoDown a (moveDown a (row,col))
    |indiceXY a (moveRight a (row,col)) == "0" || indiceXY a (moveRight a (row,col)) == "f" = [moveRight a (row,col)] ++ recorridoRight a (moveRight a (row,col))
    |indiceXY a (moveLeft a (row,col)) == "0" || indiceXY a (moveLeft a (row,col)) == "f" = [moveLeft a (row,col)] ++ recorridoLeft a (moveLeft a (row,col))
    |indiceXY a (moveUp a (row,col)) == "0" || indiceXY a (moveUp a (row,col)) == "f" = [moveUp a (row,col)] ++ recorridoUp a (moveUp a (row,col))    

