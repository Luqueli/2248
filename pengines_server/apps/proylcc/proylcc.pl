:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 



 join(Grid, NumOfColumns, Path, RGrids):-
    path_a_lista(Path,NumOfColumns,ListaPosiciones),
	sort(ListaPosiciones,ListaPosicionesSort),
 	modificar_valoresGrilla(Grid,ListaPosicionesSort,0,GrillaConCeros),
	suma_valores(ListaPosicionesSort,Grid,SumaTotal),
	proxima_potencia_de_dos(SumaTotal,Potencia),
	ultima_posicion(UltimaPos,ListaPosiciones),
	modificar_valoresGrilla(GrillaConCeros,[UltimaPos],Potencia,GrillaConPotencia),
	RGrids=[GrillaConCeros,GrillaConPotencia]
	.

%Convierte el path en lista de posiciones
    path_a_lista([],_,[]).
	
	path_a_lista([[X,Y]|Res],NumOfColumns,[ListaPos|Pos]):-
    		ListaPos is Y+X*NumOfColumns,
    		path_a_lista(Res,NumOfColumns,Pos)
	.


% Regla para modificar las posiciones de una lista a un valor dado
	modificar_valoresGrilla(Lista, Posiciones, Valor, Resultado) :-
    	modificar_valoresAux(Lista, Posiciones, Valor, 0, Resultado)
	.

% Regla auxiliar que recorre la lista y modifica las posiciones indicadas
	
	modificar_valoresAux(Lista, [], _, _, Lista).
	
	modificar_valoresAux([_|T], [], _, _, T).
	
	modificar_valoresAux([_|T], [P|Ps], Valor, P, [Valor|T2]) :-
    	P1 is P + 1,
	    modificar_valoresAux(T, Ps, Valor, P1, T2)
	.
	modificar_valoresAux([H|T], Posiciones, Valor, I, [H|T2]) :-
    	I1 is I + 1,
    	modificar_valoresAux(T, Posiciones, Valor, I1, T2)
	.

	%Regla para sumar los valores en determinadas posiciones
	suma_valores([], _, 0).
	
	suma_valores([Position|RestPositions], List, Sum) :-
			nth0(Position, List, Element),
			suma_valores(RestPositions, List, RestSum),
			Sum is Element + RestSum
	.

	%Regla para obtener la ultima poscion de una lista
	ultima_posicion(X, [X]).
	
	ultima_posicion(X, [_|T]) :- 
		ultima_posicion(X, T)
	.

	/**
	* proxima_potencia_de_dos(+Num,-Potencia).
	* Retrona la proxima potencia de 2 (-Potencia) mayor o igual al Numero (+Num). 
	*/
	proxima_potencia_de_dos(Num,Potencia):-
    	es_potencia_de_dos(Num),
    	Potencia is Num
	.

	proxima_potencia_de_dos(Num,Potencia):-
   		NumAux is Num+1,
   		proxima_potencia_de_dos(NumAux,Potencia)
	.

	/**
	*es_potencia_de_dos(+N).
	* Retorna true si el N es un numero potencia de 2 
	* y false en caso contrario.
	*/
	es_potencia_de_dos(1).

	es_potencia_de_dos(N):-
		N > 1,
    	N mod 2 =:= 0,
    	NAux is N // 2,
    	es_potencia_de_dos(NAux)
	.
	