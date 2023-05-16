:- module(proylcc, 
	[  
		join/4,
		booster/3
	]).

:-dynamic visited/1.

/*
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 * 
 */ 
join(Grilla, NumOfColumns, Path, RGrids):-
    path_a_lista(Path,NumOfColumns,ListaPosiciones),
	sort(ListaPosiciones,ListaPosicionesSort),
 	modificar_valores_grilla(Grilla,ListaPosicionesSort,0,GrillaConCeros),
	suma_valores(ListaPosicionesSort,Grilla,SumaTotal),
	proxima_potencia_de_dos(SumaTotal,Potencia),
	get_last_element(ListaPosiciones, UltimaPos),
	modificar_valores_grilla(GrillaConCeros,[UltimaPos],Potencia,GrillaConPotencia),
	get_list_without_last_pos(Path, PathSinUltimaPos),
    get_list_of_columns(PathSinUltimaPos,ListOfColumns),
    select_positions(PathSinUltimaPos,ListOfColumns, ListOfPos),
    path_a_lista(ListOfPos,NumOfColumns,ListLowerPositions),
    aplicar_gravedad(GrillaConPotencia,NumOfColumns, ListLowerPositions, GrillaConGravedad),
	RGrids=[GrillaConPotencia,GrillaConGravedad]
.

/*
*booster(+Grilla, +NumOfColumns, -GrillaRes)
* Este predicado es el encargado de realizar el efecto de eliminar todos los caminos adyacentes 
* que hayan en la Grilla y devuevle en GrillaRes la grilla con la gravedad aplicada.
*/
booster(Grilla, NumOfColumns, RGrids):-
	booster_aux(0, Grilla, NumOfColumns, [], ListPathsTemp),
    borrar_visitados,
    eliminar_caminos_invalidos(ListPathsTemp,ListPaths),
    calcular_potencias(ListPaths,Grilla,ListOfPosAndPot),
    eliminar_caminos_en_grilla(Grilla, ListPaths, GrillaConCeros),
    insert_potencias(GrillaConCeros, ListOfPosAndPot, GrillaConPotencias),
    get_list_to_gravity(ListPaths, NumOfColumns, ListToGravity),
    aplicar_gravedad(GrillaConPotencias, NumOfColumns, ListToGravity, GrillaConGravedad),
	RGrids=[GrillaConPotencias, GrillaConGravedad]
.
/*
*booster_aux(+Pos, +Grilal, +NumOfColumns, +ListOfPathsTemp, -ListPaths)
* Este predicado retorna en ListPaths una lista con todos los caminos adycentes que encuentra en la Grilla.
*/
booster_aux(Pos, Grilla, NumOfColumns, ListOfPathsTemp, ListPaths):-
    length(Grilla,SizeG),
    Pos < SizeG,
    recorrer_adyc_a_pos(Grilla, NumOfColumns, [Pos], [],PathOfPos),
    NextPos is Pos + 1,
    booster_aux(NextPos, Grilla, NumOfColumns, [PathOfPos | ListOfPathsTemp], ListPaths)
.

booster_aux(_Pos, _Grilla , _NumOfColumns ,ListOfPaths, ListOfPaths).

/*
*borrar_visitados/0
* Este predicado elimina todos las reglas dinamicas de la forma visited/1 
* que fueron agregadas hasta el momento en el programa.
*/
borrar_visitados:-
    findall(Pos, retract(visited(Pos)),_)
.

/*
 * get_list_to_gravity(+List, +NumOfColumns, -ListToGravity)
 * 
 * Este predicado se encarga de realizar las conversiones necesarias a la List 
 * para poder ser utilizada en aplciar_gravedad/4 desde el booster.
 */
get_list_to_gravity(List, NumOfColumns, ListToGravity):-
    get_concat_list(List, ListConcat),
    get_list_pos_row_col(ListConcat, NumOfColumns, ListPosRowCol),
    get_list_of_columns(ListPosRowCol, ListOfColumns),
    select_positions(ListPosRowCol, ListOfColumns, ListOfLowerPos),
    path_a_lista(ListOfLowerPos, NumOfColumns, ListToGravity)
.
/*
 * path_a_lista(+Path, +NumOfColumns, -ListPos)
 * este predicado recibe el Path 
 *   que es una lista de posiciones con elementos Posicion=[Row,Col] y 
 *  retorna en ListPos una lista con las mismas posiciones de Path pero
 *  en un formato de numero entero positivo (Pos = Col + NumOfColumns * Row).
 */
path_a_lista([],_,[]).
	
path_a_lista([[X,Y]|Res],NumOfColumns,[ListaPos|Pos]):-
    ListaPos is Y+X*NumOfColumns,
    path_a_lista(Res,NumOfColumns,Pos)
.

/*
*select_positions(+Path, +ListOfColumns, -ListOfPos)
* Recibe el Path, que es una lista de Posiciones=[Row,Col], la ListOfColumns
* una lista con los numeros de las distintas columnas que estaban en el path.
* Y retorna la lista ListOfPos que contiene correspondiente a cada columna en ListOfColumn
* su Row mas grande.
*/
select_positions(Path, [Col], [[MaxRow,Col]]):-
	findall(Row, member([Row,Col], Path), ListColumn),
   	max_list(ListColumn, MaxRow)
.

select_positions(Path, [HCol| TCol],[[MaxRow,HCol] | ListResTemp]):-
    select_positions(Path, TCol, ListResTemp),
    findall(Row, member([Row,HCol],Path), ListColumn),
    max_list(ListColumn, MaxRow)
. 

/*
*get_list_of_columns(+Path,-ListOfColumns)
* Recibe la lista Path, que es una lista de Posiciones=[Row,Col], 
* y retorna en ListOfColumns una lista con los numeros de las distintas 
* columnas que estaban en el path.
*/
get_list_of_columns(Path,ListOfColumns):-
   	findall(Col, member([_,Col],Path),ListWhitColumnsRep),
    list_to_set(ListWhitColumnsRep,ListOfColumns)
.

/*
 * get_list_pos_row_col(+ListPos, +NumOfColumns, -ListPosRowCol)
 *  Este predicado transforma la ListPos, la cual es una lista de posiciones ,
 *  en una ListPosRowCol con las mismas posiciones pero en formato de [Row,Col].
 */
get_list_pos_row_col([],_NumOfColumns, []).

get_list_pos_row_col([Pos | RemPos], NumOfColumns, [[Row,Col]|ListRes]):-
    get_list_pos_row_col(RemPos, NumOfColumns, ListRes),
    Row is Pos // NumOfColumns,
    Col is Pos - (Row * NumOfColumns)
.
/*
 * get_concat_list(+ListOfPaths, -ListConcat)
 * Concatena las sublistas de ListOfPaths en una unica lista ListConcat.
 */               
get_concat_list([],[]).

get_concat_list([Path | RemPaths], ListConcat):-
   get_concat_list(RemPaths, ListToGravityTemp),
   get_list_without_first_pos(Path, PathWFP),
   append(PathWFP, ListToGravityTemp, ListConcat)
.
/*
*get_list_without_first_pos(+List, -ListWFP) 
* Retorna la List en ListWFP sin la primera posicion.
*/
 get_list_without_first_pos([_H | T], T).

/*
 * insert_potencias(+Grilla, +ListPosPot, -GrillaRes)
 * 
 */
insert_potencias(Grilla, [], Grilla).

insert_potencias(Grilla, [[Pos,Pot] | RemElems], GrillaRes):-
    insert_potencias(Grilla, RemElems, GrillaResTemp),
    modificar_valores_grilla(GrillaResTemp, [Pos], Pot, GrillaRes)
.

/*
 * eliminar_caminos_en_grilla(+Grilla, +ListOfPaths, -GrillaRes)
 *
 */
eliminar_caminos_en_grilla(Grilla, [], Grilla).

eliminar_caminos_en_grilla(Grilla, [Path | RemPaths], GrillaRes):-
    eliminar_caminos_en_grilla(Grilla, RemPaths, GrillaResTemp),
    sort(Path,PathSorted),
    modificar_valores_grilla(GrillaResTemp, PathSorted, 0, GrillaRes)
.

/*
 * calcular_potencias(+ListOfPaths, +Grilla, -ListPosPot)
 * 
 * Este predicado retorna la potencia de cada Path de ListOfPaths en ListPosPot.
 * ListPosPot: esta lista contiene elmentos de la forma [Pos, Potencia] donde Pos
 * 			   es la posicion en la grilla donde corresponde insertar la Potencia. 
 */
calcular_potencias([], _Grilla, []).

calcular_potencias([Path | RemPaths], Grilla, [[Pos,Potencia]|ListRes]):-
	calcular_potencias(RemPaths, Grilla, ListRes),
    suma_valores(Path,Grilla,Sumatoria),
    proxima_potencia_de_dos(Sumatoria,Potencia),
    nth0(0,Path,Pos)
.

/*
* recorrer_adyc_a_pos(+Grilla, +NumOfColumns, +ListPend, +ListVisited, -ListPos)
* Recorre la grilla en busca de los adyacentes de igual valor
* a las posiciones en la ListPend
* y retorna las mismas en -ListPos
*
*/
recorrer_adyc_a_pos(_Grilla, _NumOfColumns, [], ListVisited, ListVisited).

recorrer_adyc_a_pos(Grilla, NumOfColumns, ListPend, ListVisited, ListPos):-
    ListPend= [Pos | RPos],
    assert(visited(Pos)),
    findall(P,
            (ady(Pos, Grilla, NumOfColumns, P),
             is_equals(Pos, P, Grilla),
             not(is_visited(P)),
             not(member(P, ListPend)),
             not(member(P, ListVisited))
            ),
           ListOfAdEq),
    append(ListOfAdEq, RPos, NPend),
    recorrer_adyc_a_pos(Grilla, NumOfColumns, NPend, [Pos | ListVisited], ListPos)
.

/*
* is_visited(+Pos)
* Retorna true si Pos esta visitado.Caso contrario retorna false.
*/
is_visited(Pos):-
    visited(Pos)
.

/*
*ady(+Pos, +Grilla, +NumOfColumns, -PosAd)
* Este predicado retorna en PosAd una posicion adyacente 
* a Pos según la ubicación de Pos en la Grilla.
*/
ady(Pos, Grilla, NumOfColumns, PosAd):-
   length(Grilla, Size),
   StartLastRow is Size - NumOfColumns,
   (
   		(not((Pos+1) mod NumOfColumns =:= 0), PosAd is Pos + 1);
   		(not(Pos mod NumOfColumns =:= 0) ,PosAd is Pos - 1);
   		(not(Pos >= StartLastRow), PosAd is Pos + NumOfColumns);
   		(not(Pos < NumOfColumns), PosAd is Pos - NumOfColumns);
   		(   
        	not((Pos mod NumOfColumns =:= 0) ;	(Pos < NumOfColumns)),
            PosAd is Pos - NumOfColumns - 1
        );
   		(
        	not((Pos mod NumOfColumns =:= 0); (Pos >=StartLastRow)),
        	PosAd is Pos + NumOfColumns - 1
		);
   		(
        	not((Pos<NumOfColumns);((Pos +1) mod NumOfColumns =:= 0)),
            PosAd is Pos - NumOfColumns + 1
        );
   		(
        	not((Pos >= StartLastRow);((Pos+1) mod NumOfColumns =:= 0)),
        	PosAd is Pos + NumOfColumns + 1 
        )
   )
.

/*
* is_equals(+Pos, +PosAd, +Grilla)
* Este predicado retorna true si dos Pos y PosAd tienen el mismo elmento en la Grilla.
* Caso contrario retorna false.
*/
is_equals(Pos, PosAd , Grilla):-
    nth0(Pos, Grilla, Elem),
    nth0(PosAd, Grilla, Elem)
.

/*
*eliminar_caminos_invalidos(+ListOfPaths, -ListRes)
* Elimina los caminos de longitud 1 de ListOfPaths yb retorna el resultado en ListRes.
*/
eliminar_caminos_invalidos([],[]).

eliminar_caminos_invalidos([H | T], ListaRes):-
    eliminar_caminos_invalidos(T, ListaResTemp),
    length(H,HSize),
    agregar(H, HSize, ListaResTemp, ListaRes)
.

agregar(_H, 1, Lista, Lista).

agregar(H, _HSize, List, ListRes):-
    nth0(0, ListRes, H, List)
.


/*
*modificar_valores_grilla(+Lista, +Posiciones, +Valor, -Resultado)
*Regla para modificar las posiciones de una lista a un valor dado
*
*/
modificar_valores_grilla(Lista, Posiciones, Valor, Resultado) :-
	modificar_valores_aux(Lista, Posiciones, Valor, 0, Resultado)
.
	
modificar_valores_aux(Lista, [], _, _, Lista).
	
modificar_valores_aux([_|T], [], _, _, T).
	
modificar_valores_aux([_|T], [P|Ps], Valor, P, [Valor|T2]) :-
    P1 is P + 1,
	modificar_valores_aux(T, Ps, Valor, P1, T2)
.

modificar_valores_aux([H|T], Posiciones, Valor, I, [H|T2]) :-
    I1 is I + 1,
    modificar_valores_aux(T, Posiciones, Valor, I1, T2)
.

/* 
* suma_valores(+ListOfPos, +Grid, -Sumatoria)
*Regla para sumar los valores en determinadas posiciones
*
*/	
suma_valores([], _, 0).
	
suma_valores([Position|RestPositions], Grid, Sum) :-
	nth0(Position, Grid, Element),
	suma_valores(RestPositions, Grid, RestSum),
	Sum is Element + RestSum
.
/*
*get_last_element(+List,-Elem)
* Retorna el ultimo elmento deList en Elem.
*/	
get_last_element(List, Elem) :- 
	length(List, LSize),
    LastPos is LSize  - 1,
    nth0(LastPos, List, Elem)
.

/*
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

/*
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

/*
 * get_list_without_last_pos(+List, -ListRes).
 * Recibe la lista List y retorna en ListaRes la lista List
 * sin su ultimo elemento.
 * 
 */
get_list_without_last_pos(List, ListRes):-
    length(List,Size),
    LastPosOfList is Size-1,
    nth0(LastPosOfList,List,Elem),
    nth0(LastPosOfList, List, Elem, ListRes)
.

/*
* aplicar_gravedad(+Grilla, +NumOfColumns, +ListLowerPositions, -GrillaConGravedad)
* Este metodo descompone recursivamente la lista de ListLowerPositions,
* y un vez que llega a su caso base, donde la lista es vacia, por cada elemento de 
* ListLowerPositions (desde el ultimo elemento hasta el primero) 
* llamara a aplicar_gravedad_a_columna/4 quien retorna un grilla (GrillaTemp) 
* con el efecto de gravedad aplicado a la columna por la cual fue llamado.
* Una vez hecho esto por cada elemento de ListLowerPosition, GrillaConGravedad
* contendra la grilla resultante con la graverdad y generacion random 
* de los nuevos bloques aplicada.
*
*/
aplicar_gravedad(Grilla, _NumOfColumns, [], Grilla).

aplicar_gravedad(Grilla, NumOfColumns, [Pos | RemPos], GrillaConGravedad):-
    aplicar_gravedad(Grilla, NumOfColumns, RemPos, GrillaTemp),
    aplicar_gravedad_a_columna(GrillaTemp, NumOfColumns, Pos, GrillaConGravedad)
    
.

/*
* aplicar_gravedad_a_columna(+Grilla, +NumOfColumns, +LowerPos, -GrillaComGravedad).
* Este predicado aplica la gravedad y genera los nuevos numeros random
* a la columna correspondiente a LowerPos, y retorna la grilla con las modificaciones en GrillaConGravedad.
*/
aplicar_gravedad_a_columna(Grilla, NumOfColumns, LowerPos, GrillaConGravedad):-
    nth0(LowerPos, Grilla, Elem),
    LowerPos>=NumOfColumns,
    (   
    	(
    	Elem=:= 0,
        aplicar_gravedad_a_bloque(Grilla, NumOfColumns, LowerPos,GrillaRes),
    	aplicar_gravedad_a_columna(GrillaRes, NumOfColumns, LowerPos, GrillaConGravedad)
    	)
    	;   
    	( 
   	    Elem =\= 0,
        LowerPosAux is LowerPos-NumOfColumns,
    	aplicar_gravedad_a_columna(Grilla, NumOfColumns, LowerPosAux, GrillaConGravedad)
    	)	
    )
.

aplicar_gravedad_a_columna(Grilla, _NumOfColumns, LowerPos, GrillaRes):-
    nth0(LowerPos,Grilla, Elem),
    (
        (
        Elem=:=0,
        generar_valor_random(NewValue),
        nth0(LowerPos, Grilla, Elem, Remainder),
        nth0(LowerPos, GrillaRes, NewValue, Remainder)
        )
        ;
        (
        Elem=\=0,
        GrillaRes = Grilla
        )
    )
.

/*
*aplicar_gravedad_a_bloque(+Grilla, +NumOfColumns, +Pos, -GrillaRes).
* Este predicado aplica la gravedad a un unico boque, intercambiando el bloque Pos
* con su bloque superior hasta llegar a la primera fila de la Grilla. Luego genera un numero nuevo random
* a insertar en dicha Pos.
*/
aplicar_gravedad_a_bloque(Grilla, NumOfColumns, Pos, GrillaRes):-
    Pos>=NumOfColumns, % Si no estoy en la 1era fila.
    PosBloqueDeArriba is Pos - NumOfColumns, %Calculo la pos del bloque de arriba.
    nth0(PosBloqueDeArriba, Grilla, Elem), %Obtengo el Elem a bajar. 
    nth0(Pos, Grilla, 0, GrillaAux), %Borro el 0 de Pos.
    nth0(Pos, GrillaAux2, Elem, GrillaAux), %Inserto Elem en Pos
    nth0(PosBloqueDeArriba, GrillaAux2, Elem, GrillaAux3), %Borro Elem de PosBloqueDeArriba
    nth0(PosBloqueDeArriba, GrillaAux4, 0, GrillaAux3), %Inserto 0 en PosBloqueDeArriba
    aplicar_gravedad_a_bloque(GrillaAux4, NumOfColumns, PosBloqueDeArriba, GrillaRes)
.

aplicar_gravedad_a_bloque(Grilla, _NumOfColumns, Pos, GrillaRes):-
    nth0(Pos,Grilla,Elem),
    (   
    	(
    	Elem=:= 0,
    	generar_valor_random(NewValue),
    	nth0(Pos,Grilla,0,Remainder),% Elimino el 0 que ya esta en la primera fila.
    	nth0(Pos, GrillaRes, NewValue, Remainder) % Inserto un nuevo bloque random.
    	)
    	;   
    	(
    	Elem=\=0,
    	GrillaRes= Grilla
    	)
    )
.

/*
*generar_valor_random(+R).
* Retorna en R un numero al azar.
* que es potencia de dos, en el rango [2-64].
*
*/
generar_valor_random(R) :-
	random_member(R, [2, 4, 8, 16, 32, 64])
.