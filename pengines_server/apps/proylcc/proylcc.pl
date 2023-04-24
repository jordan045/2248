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


% generate(+Grid,Index,RGrid)
/* generate([],40,Grid).
generate([0|T],I,RGrid) :-
	NI is I+1,
	random(0,8,Exp),
	NewNumber is 2**Exp,
	replace([0|T],I,NewNumber,RGrid),
	generate(T,NI,RGrid).
generate([_|T],I,RGrid) :-
	NI is I+1,
	generate(T,NI,RGrid). */

% Recorrer lista, si hay un 0 genera un random y replace

join(Grid, _NumOfColumns, Path, RGrids):-
	set_all_cero(Grid,Path,Aux,0),
	add0s(Aux,0,NList),
	gravity(Aux,NList,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [Aux,RGravity,RGenerate].

%random/3