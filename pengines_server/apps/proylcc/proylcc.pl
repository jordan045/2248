:- module(proylcc, 
	[  
		join/4
	]).

% replace(+List,+Index,+Value,-NewList).
replace([_|T], 0, X,[X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% get_index(+Path,+NoC,-RIndex)
% Genera el índice en la lista a partir de un valor [X,Y]
get_index([X|[Y]],NoC,RIndex) :- RIndex is (X*NoC)+Y.

% get_value(+Grid,+Path,NoC,-Value)
% Extrae el valor de una celda a partir de su valor [X,Y]
get_value(Grid,[X|[Y]],NoC,Value) :- 
	get_index([X|[Y]],NoC,RIndex),
	nth0(RIndex,Grid,Value).


result(SumaTotal, Mult, Bloque) :- 
	SumaTotal =< Mult,
	Bloque is Mult.
result(SumaTotal, Mult, Bloque)  :-
	BloqueAux is Mult * 2,
	result(SumaTotal, BloqueAux, Bloque).


% make_move(+Grid,+Path,+Noc,-RGrid,-SumaTotal)
make_move(Grid,[],_NoC,Grid,_).
make_move(Grid,[P],NoC,RGrid,SumaTotal) :-
	get_index(P,NoC,RIndex),
	get_value(Grid,P,NoC,Valor),
	SumaTotalAux is SumaTotal + Valor,
	result(SumaTotalAux, 1, Bloque),
	replace(Grid, RIndex, Bloque, Aux),
	make_move(Aux,[],NoC,RGrid,SumaTotalAux).
make_move(Grid,[P|Ps],NoC,RGrid,SumaTotal):-
	get_index(P,NoC,RIndex),
	get_value(Grid,P,NoC,Valor),
	replace(Grid, RIndex, 0, Aux),
	SumaTotalAux is SumaTotal + Valor,
	make_move(Aux,Ps,NoC,RGrid,SumaTotalAux).

% swap(+Grid,+IndexA,+IndexB,-RGrid)
% Intercambia dos celdas de Grid a partir de los índices, devuelve RGrid
swap(Grid,IA,IB,RGrid) :-
	nth0(IA,Grid,Aux1),
	nth0(IB,Grid,Aux2),
	replace(Grid,IA,Aux2,AuxGrid),
	replace(AuxGrid,IB,Aux1,RGrid).

% create_list_zeros(+Grid,+Index,-List)
% Genera una lista que contiene los índices con valor cero en Grid
create_list_zeros([],40,[]).
create_list_zeros([0|T],I,[I|List]) :-
	NI is I+1,
	create_list_zeros(T,NI,List).
create_list_zeros([_|T],I,List) :-
	NI is I+1,
	create_list_zeros(T,NI,List).	

% recursive_swap(+Grid,+Value,+Index,RGrid)
recursive_swap(Grid,_,I,_NoC,Grid) :-
    I < 0.
recursive_swap(Grid,X,NI,NoC,RGrid) :-
    swap(Grid,X,NI,Aux),
	NI2 is NI-NoC,
	recursive_swap(Aux,NI,NI2,NoC,RGrid).	

% gravity(+Grid,+List0,-RGrid)
% Intercambia recursivamente en la grilla para simular la gravedad
gravity(Grid,[],_NoC,Grid).
gravity(Grid,[X|Xs],NoC,RGrid) :-
	NI is X-NoC,
	recursive_swap(Grid,X,NI,NoC,RRGrid),
	gravity(RRGrid,Xs,NoC,RGrid).

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
not_member(X, [H|T]) :-
     X \= H,
    not_member(X, T).

check_up(I,[-6,-5,-4]) :- I < 5.
check_up(I,[]) :- 		  I > 4.

check_down(I,[]):-          I < 35.
check_down(I,[4,5,6]):-  	I > 34.

check_left(I,[]):-        Mod is I mod 5, Mod =\= 0.
check_left(I,[-6,-1,4]):- Mod is I mod 5, Mod is 0.

check_right(I,[]):-       Mod is I mod 5, Mod =\= 4.
check_right(I,[-4,1,6]):- Mod is I mod 5, Mod is 4.

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

recursive_find(_Grid,[],_Visited,NAdy, NAdy).
recursive_find(Grid,[X|Xs],Visited,NAdy, ReturnAdy):-
	valid_moves(X,NPossible),
	find_adyacencies(Grid,X,NAdy,NPossible,Visited,RAdy),
    union(NAdy,RAdy,NewAdy),
	recursive_find(Grid,Xs,Visited,NewAdy, ReturnAdy).

add_index(Ady,_,[],Ady).
add_index(Ady,Index,_AuxAdy,Moves) :-
    union(Ady,[Index],Moves).

merge_adys(_,Moves,[],_Visited,Moves).
merge_adys(Grid,Moves,AuxAdy,Visited,ReturnAdy) :-
    union(Moves,AuxAdy,RAdy),
	recursive_find(Grid,AuxAdy,Visited,RAdy,ReturnAdy).

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
                not_member(NI,Visited)),AuxAdy),
    add_index(Ady,Index,AuxAdy,Moves),
	merge_adys(Grid,Moves,AuxAdy,Visited,RAdy).

% list_booster(Grid,Grid,0,[],LoL)
% list_booster(+Grid,+GridConsume,Index,Ady,LoL)
list_booster(_Grid,[],_I,_Ady,_Visited,[]).
list_booster(Grid,[_X|Xs],I,Ady,Visited,[RAdy|LoL]) :-
	valid_moves(I,Possible),
	find_adyacencies(Grid,I,Ady,Possible,Visited,RAdy),
	union(RAdy,Visited,VisitedAUX),
	NI is I+1,
	list_booster(Grid,Xs,NI,[],VisitedAUX,LoL).

% make_move_booster(+Grid,+Path,-RGrid,-SumaTotal)
make_move_booster(Grid,[],Grid,_).
make_move_booster(Grid,[P],RGrid,SumaTotal) :-
	nth0(P,Grid,Value),
	SumaTotalAux is SumaTotal + Value,
	result(SumaTotalAux, 1, Bloque),
	replace(Grid, P, Bloque, Aux),
	make_move_booster(Aux,[],RGrid,SumaTotalAux).
make_move_booster(Grid,[P|Ps],RGrid,SumaTotal):-
	nth0(P,Grid,Value),
	replace(Grid, P, 0, Aux),
	SumaTotalAux is SumaTotal + Value,
	make_move_booster(Aux,Ps,RGrid,SumaTotalAux).

join_booster(Grid, [], NList, NumOfColumns, RGrids):-
	gravity(Grid,NList,NumOfColumns,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [Grid,RGravity,RGenerate].
join_booster(Grid, [X|Xs], NList, NumOfColumns, RGrids):-
	make_move_booster(Grid, X, Aux, 0),
	create_list_zeros(Aux,0,NNList),
	union(NList,NNList,List),
	join_booster(Aux, Xs,List, NumOfColumns, RGrids).

booster(Grid, NumOfColumns, RGrid):-
	list_booster(Grid,Grid,0,[],[],LoL),
	join_booster(Grid, LoL, [],NumOfColumns, RGrid).

join(Grid, NumOfColumns, Path, RGrids):-
	make_move(Grid,Path,NumOfColumns,RMove,0),
	create_list_zeros(RMove,0,NList),
	gravity(RMove,NList,NumOfColumns,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [RMove,RGravity,RGenerate].

/* 
trace,
list_booster([2,4,64,64,2,
	32,4,32,16,4,
	16,4,16,16,16,
	16,64,2,32,32,
	64,2,64,32,64,
	32,2,64,32,4,	
	64,4,64,32,16,
	64,8,16,2,32],[2,4,64,64,2,
	32,4,32,16,4,
	16,4,16,16,16,
	16,64,2,32,32,
	64,2,64,32,64,
	32,2,64,32,4,	
	64,4,64,32,16,
	64,8,16,2,32],0,[],[],LoL).
 */