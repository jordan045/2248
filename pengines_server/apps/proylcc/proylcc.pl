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

numi([X|Y],RIndex) :- RIndex = (X*5)+Y.

% set_all_cero(Grid,Path,RGrids)
set_all_cero(Grid,[],Grid,456).

set_all_cero(Grid,[P],RGrid,SumaTotal) :-
	numi(P,RIndex),
%	nth0(RIndex,Grid,Suma),
%	SumaTotal = SumaTotal + Suma,
	replace(Grid, RIndex, SumaTotal, Aux),
	set_all_cero(Aux,[],RGrid,SumaTotal).

set_all_cero(Grid,[P|Ps],RGrid,SumaTotal):-
	numi(P,RIndex),
%	nth0(RIndex,Grid,Suma),
%	SumaTotal = SumaTotal + Suma,
	replace(Grid, RIndex, 0, Aux),
	set_all_cero(Aux,Ps,RGrid,SumaTotal).


join(Grid, _NumOfColumns, Path, RGrids):-
	set_all_cero(Grid,Path,Aux,SumaTotal),
	RGrids = [Aux,Aux].









%	Grid = [N | Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
%	N2 is N * 2,		% por una implementación válida.
%	RGrids = [[0 | Ns], [N2 | Ns]].