:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

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


join(Grid, _NumOfColumns, Path, RGrids):-
	set_all_cero(Grid,Path,Aux,0),
	RGrids = [Aux,Aux].

%random/3