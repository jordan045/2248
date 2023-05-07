:- module(proylcc, 
	[  
		join/4
	]).

% replace(+List,+Index,+Value,-NewList).
replace([_|T], 0, X,[X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% get_index(+Path,-RIndex)
% Genera el índice en la lista a partir de un valor [X,Y]
get_index([X|[Y]],RIndex) :- RIndex is (X*5)+Y.

% get_value(+Grid,+Path,-Value)
% Extrae el valor de una celda a partir de su valor [X,Y]
get_value(Grid,[X|[Y]],Value) :- %EXTRAE CELDA
	get_index([X|[Y]],RIndex),
	nth0(RIndex,Grid,Value).


resultado(SumaTotal, Mult, Bloque) :- 
	SumaTotal =< Mult,
	Bloque is Mult.
resultado(SumaTotal, Mult, Bloque)  :-
	BloqueAux is Mult * 2,
	resultado(SumaTotal, BloqueAux, Bloque).


% set_all_cero(+Grid,+Path,-RGrid,-SumaTotal)
set_all_cero(Grid,[],Grid,_).
set_all_cero(Grid,[P],RGrid,SumaTotal) :-
	get_index(P,RIndex),
	get_value(Grid,P,Valor),
	SumaTotalAux is SumaTotal + Valor,
	resultado(SumaTotalAux, 1, Bloque),
	replace(Grid, RIndex, Bloque, Aux),
	set_all_cero(Aux,[],RGrid,SumaTotalAux).
set_all_cero(Grid,[P|Ps],RGrid,SumaTotal):-
	get_index(P,RIndex),
	get_value(Grid,P,Valor),
	replace(Grid, RIndex, 0, Aux),
	SumaTotalAux is SumaTotal + Valor,
	set_all_cero(Aux,Ps,RGrid,SumaTotalAux).

% swap(+Grid,+IndexA,+IndexB,-RGrid)
% Intercambia dos celdas de Grid a partir de los índices, devuelve RGrid
swap(Grid,IA,IB,RGrid) :-
	nth0(IA,Grid,Aux1),
	nth0(IB,Grid,Aux2),
	replace(Grid,IA,Aux2,AuxGrid),
	replace(AuxGrid,IB,Aux1,RGrid).

% create_list_zeros(+Grid,+Index,-List)
% Genera una lista que contiene los índices con valor cero en Grid
create_list_zeros([],40,_).
create_list_zeros([0|T],I,[I|List]) :-
	NI is I+1,
	create_list_zeros(T,NI,List).
create_list_zeros([_|T],I,List) :-
	NI is I+1,
	create_list_zeros(T,NI,List).	

% recursive_swap(+Grid,+Value,+Index,RGrid)
recursive_swap(Grid,_,I,Grid) :-
    I < 0.
recursive_swap(Grid,X,NI,RGrid) :-
    swap(Grid,X,NI,Aux),
	NI2 is NI-5,
	recursive_swap(Aux,NI,NI2,RGrid).	

% gravity(+Grid,+List0,-RGrid)
% Intercambia recursivamente en la grilla para simular la gravedad
gravity(Grid,[],Grid).
gravity(Grid,[X|Xs],RGrid) :-
	NI is X-5,
	recursive_swap(Grid,X,NI,RRGrid),
	gravity(RRGrid,Xs,RGrid).

% generate(+List,+Index,-RList)
generate([],40,[]).
generate([0|T],I,[NewNumber|RGrid]) :-
	NI is I+1,
	random(1,8,Exp),
	NewNumber is 2**Exp,
	generate(T,NI,RGrid).
generate([H|T],I,[H|RGrid]) :-
	NI is I+1,
	generate(T,NI,RGrid).

equal(Grid, NI, I):-
	nth0(NI, Grid, V),
	nth0(I, Grid, Z),
	V is Z.

not_member(_, []).
not_member(X, [Head|Tail]) :-
     X \= Head,
    not_member(X, Tail).

% Primero tendremos un metodo shell que ira consumiendo la lista
% El método recursivo irá pidiendo todas las adyacencias

check_up(I,[-6,-5,-4]) :- I < 5.
check_up(I,[]) :- 		  I > 4.

check_down(I,[]):-          I < 35.
check_down(I,[4,5,6]):-  	I > 34.

check_left(I,[]):-        Mod is I mod 5, Mod =\= 0.
check_left(I,[-6,-1,4]):- Mod is I mod 5, Mod is 0.

check_right(I,[]):-       Mod is I mod 5, Mod =\= 4.
check_right(I,[-4,1,6]):- Mod is I mod 5, Mod is 4.

% valid_moves(+Index,-List)
% True cuando List es una lista conteniendo los índices relativos válidos a la grilla para Index
valid_moves(Index,List) :-
	AllList = [-6,-5,-4,-1,1,4,5,6],
	check_up(Index,LU),    
	check_down(Index,LD),  
	check_right(Index,LR), 
	check_left(Index,LL),  
	union(LU,LD,Aux),
	union(Aux,LR,Aux2),
	union(Aux2,LL,NList),
	subtract(AllList,NList,List).



% add_index(+Ady,+Index,+AuxAdy,-Moves)
% Agrega el indice actual a la lista de adyacencia si el findall no es vacío
add_index(Ady,_,[],Ady).
add_index(Ady,Index,_AuxAdy,Moves) :-
    union(Ady,[Index],Moves).

merge_adys(_Grid,Moves,[],_Visited,Moves).
merge_adys(Grid,Moves,AuxAdy,Visited,RAdy) :-
    union(Moves,AuxAdy,RAdy),
	recursive_find(Grid,AuxAdy,Visited,RAdy).

% Realiza la recursión de find_adyacencies
recursive_find(_Grid,[],_Visited,[]).
recursive_find(Grid,[X|Xs],Visited,RAdy):-
	valid_moves(X,Possible),
	find_adyacencies(Grid,X,[X|Xs],Possible,Visited,NewAdy),
	recursive_find(Grid,Xs,Visited,RRAdy),
	union(RRAdy,NewAdy,RAdy).

% find_adyacencies(+Grid,+Index,+Ady,+Possible,+Visited,-RAdy)
% - Grid: La grilla del juego
% - Index: El índice sobre el cual se buscaran adyacencias válidas
% - Ady: La lista de adyacencia local
% - Possible: La lista con las jugadas válidas para el indice actual
% - Visited: La lista con todos los elementos visitados hasta el momento
% - RAdy: La nueva lista de adyacencia local
find_adyacencies(Grid,Index,Ady,Possible,Visited,RAdy) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal(Grid,NI,Index), 
				not_member(NI,Ady),
				not_member(NI,Visited)),Found),
	add_index(Ady,Index,Found,ActualSet),
    union(ActualSet,Visited,VisitedAux),
    recursive_find(Grid,Found,VisitedAux,RAdy).
    /*¨union(ActualSet,NewAdy,RAdy). */
	/*merge_adys(Grid,ActualSet,Found,Visited,RAdy).*/

% list_booster(Grid,Grid,0,[],LoL)
% list_booster(+Grid,+GridConsume,Index,Ady,LoL)
list_booster(_Grid,[],_I,_Ady,_Visited,[]).
list_booster(Grid,[_X|Xs],I,[],Visited,LoL) :-
	valid_moves(I,Possible),
	find_adyacencies(Grid,I,[],Possible,Visited,RAdy),
    union(RAdy,Visited,VisitedAux),
	NI is I+1,
	list_booster(Grid,Xs,NI,[],VisitedAux,LoL).
list_booster(Grid,[_X|Xs],I,Ady,Visited,[Ady|LoL]) :-
	valid_moves(I,Possible),
	find_adyacencies(Grid,I,Ady,Possible,Visited,RAdy),
    union(RAdy,Visited,VisitedAux),
	NI is I+1,
	list_booster(Grid,Xs,NI,[],VisitedAux,LoL).


% set_all_cero_booster(+Grid,+Path,-RGrid,-SumaTotal)
set_all_cero_booster(Grid,[],Grid,_).
set_all_cero_booster(Grid,[P],RGrid,SumaTotal) :-
	nth0(P,Grid,Value),
	SumaTotalAux is SumaTotal + Value,
	resultado(SumaTotalAux, 1, Bloque),
	replace(Grid, P, Bloque, Aux),
	set_all_cero_booster(Aux,[],RGrid,SumaTotalAux).
set_all_cero_booster(Grid,[P|Ps],RGrid,SumaTotal):-
	nth0(P,Grid,Value),
	replace(Grid, P, 0, Aux),
	SumaTotalAux is SumaTotal + Value,
	set_all_cero_booster(Aux,Ps,RGrid,SumaTotalAux).

join_booster(Grid, [], NList, RGrids):-
	gravity(Grid,NList,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [Grid,RGravity,RGenerate].
join_booster(Grid, [X|Xs], NList, RGrids):-
	set_all_cero_booster(Grid, X, Aux, 0),
	create_list_zeros(Aux,0,NList),
	join_booster(Aux, Xs,NList, RGrids).

booster(Grid, RGrid):-
	list_booster(Grid,Grid,0,[],[],LoL),
	join_booster(Grid, LoL, _, RGrid).

join(Grid, _NumOfColumns, Path, RGrids):-
	set_all_cero(Grid,Path,Aux,0),
	create_list_zeros(Aux,0,NList),
	gravity(Aux,NList,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [Aux,RGravity,RGenerate].

/* 
trace,
create_list_booster([64,64,64,64,64,64,4,4,4,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64],0,LoL,_).
 */