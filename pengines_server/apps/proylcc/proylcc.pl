:- module(proylcc, 
	[  
		join/4
	]).

% replace(+List,+Index,+Value,-NewList).
replace([_|T], 0, X,[X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

numi([X|[Y]],RIndex) :- RIndex is (X*5)+Y.

celda(Grid,[X|[Y]],Celda) :- %EXTRAE CELDA
	numi([X|[Y]],RIndex),
	nth0(RIndex,Grid,Celda).

resultado(SumaTotal, Mult, Bloque) :- 
	SumaTotal =< Mult,
	Bloque is Mult.
	
resultado(SumaTotal, Mult, Bloque)  :-
	BloqueAux is Mult * 2,
	resultado(SumaTotal, BloqueAux, Bloque).


% set_all_cero(Grid,Path,RGrids) // podria no estar
set_all_cero(Grid,[],Grid,SumaTotal).

%eliminar este caso para hacerlo aparte
set_all_cero(Grid,[P],RGrid,SumaTotal) :-
	numi(P,RIndex),
	celda(Grid,P,Valor),
	SumaTotalAux is SumaTotal + Valor,
	resultado(SumaTotalAux, 1, Bloque),
	replace(Grid, RIndex, Bloque, Aux),
	set_all_cero(Aux,[],RGrid,SumaTotalAux).

set_all_cero(Grid,[P|Ps],RGrid,SumaTotal):-
	numi(P,RIndex),
	celda(Grid,P,Valor),
	replace(Grid, RIndex, 0, Aux),
	SumaTotalAux is SumaTotal + Valor,
	set_all_cero(Aux,Ps,RGrid,SumaTotalAux).

intercambiar(Grid,IndexA,IndexB,RGrid) :-
	nth0(IndexA,Grid,Aux1),
	nth0(IndexB,Grid,Aux2),
	replace(Grid,IndexA,Aux2,AuxGrid),
	replace(AuxGrid,IndexB,Aux1,RGrid).


% add0s(+Grid,-List)
add0s([],40,List).
add0s([0|T],I,[I|List]) :-
	NI is I+1,
	add0s(T,NI,List).
add0s([_|T],I,List) :-
	NI is I+1,
	add0s(T,NI,List).	

intercambio_recursivo(Grid,_,I,Grid) :-
    I < 0.
intercambio_recursivo(Grid,X,NI,RGrid) :-
    intercambiar(Grid,X,NI,Aux),
	NI2 is NI-5,
	intercambio_recursivo(Aux,NI,NI2,RGrid).	

% gravity(Grid,List0,RGrid)
gravity(Grid,[],Grid).
gravity(Grid,[X|Xs],RGrid) :-
	NI is X-5,
	intercambio_recursivo(Grid,X,NI,RRGrid),
	gravity(RRGrid,Xs,RGrid).

generate([],40,[]).
generate([0|T],I,[NewNumber|RGrid]) :-
	NI is I+1,
	random(1,8,Exp),
	NewNumber is 2**Exp,
	generate(T,NI,RGrid).
generate([H|T],I,[H|RGrid]) :-
	NI is I+1,
	generate(T,NI,RGrid).

sonIguales(Grid, I, X):-
	nth0(I, Grid, V),
	V is X.

% formarListaIguales(+Grid, +Index, +Value -LoL)
% Devuelve una lista de listas, donde cada una de ellas está formada por los indices
% de los cubos adyacentes con igual valor

check_up(I):-
	I > 4.
check_down(I):-
	I < 35.
check_left(I):-
	Mod is I mod 5,
	Mod =\= 0.
check_right(I):-
	Mod is I mod 5,
	Mod =\= 4.


formarListaIguales([],40,[]).	
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_up(I),
	check_left(I),
	X > 0,
	NI is I-6,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_up(I),
	X > 0,
	NI is I-5,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_right(I),
	check_up(I),
	X > 0,
	NI is I-4,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_left(I),
	X > 0,
	NI is I-1,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_right(I),
	X > 0,
	NI is I+1,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_down(I),
	check_left(I)
	X > 0,
	NI is I+4,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_down(I),
	X > 0,
	NI is I+5,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,[[I]|T]):-
	check_down(I),
	check_right(I),
	X > 0,
	NI is I+6,
	sonIguales([X|Xs],NI,X),
	formarListaIguales(Xs, NI, T).
formarListaIguales([X|Xs],I,T) :-
	NI is I+1,
	formarListaIguales(Xs,NI,T).

booster(Grid, RGrid):-
	formarListaIguales(Grid, 0, LoL).

%Lo que nos quedaria hacer ahora es, para cada lista en la LoL
%	Simular un Join
%	Vamos vaciando la LoL, llamando a Set_all_cero (path = cabeza de LoL) y a add0s
%	Una vez hecho esto, aplicamos gravedad y generamos nuevos bloques.
	

/*
-6 -5 -4
-1  * +1
 4  5  6
*/

join(Grid, _NumOfColumns, Path, RGrids):-
	formarListaIguales(Grid,0,LoL),
	set_all_cero(Grid,Path,Aux,0),
	add0s(Aux,0,NList),
	gravity(Aux,NList,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [Aux,RGravity,RGenerate].

%random/3