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

% result(+SumaTotal,+Mult,-Bloque)
% Calcula la potencia de 2 a utilizar para obtener el resultado de una jugada determinada
result(SumaTotal, Mult, Bloque) :- 
	SumaTotal =< Mult,
	Bloque is Mult.
result(SumaTotal, Mult, Bloque)  :-
	BloqueAux is Mult * 2,
	result(SumaTotal, BloqueAux, Bloque).


% make_move(+Grid,+Path,+Noc,-RGrid,-SumaTotal)
% Dado un path de longitud N, inserta valores vacios en las primeras N-1 posiciones, y genera un bloque
% Con el resultado pertinente en la posicion N
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
create_list_zeros([],40,[]):- !.
create_list_zeros([0|T],I,[I|List]) :-
	NI is I+1,
	create_list_zeros(T,NI,List),
    !.
create_list_zeros([_|T],I,List) :-
	NI is I+1,
	create_list_zeros(T,NI,List).	

% recursive_swap(+Grid,+Value,+Index,-RGrid)
% Intercambia el valor de una celda vacia con el de su inmediata superior 
% recursivamente, hasta que la celda vacia quede en la parte superior de la grilla
recursive_swap(Grid,_,I,_NoC,Grid) :-
    I < 0,!.
recursive_swap(Grid,X,NI,NoC,RGrid) :-
    swap(Grid,X,NI,Aux),
	NI2 is NI-NoC,
	recursive_swap(Aux,NI,NI2,NoC,RGrid).	

% gravity(+Grid,+List0,-RGrid)
% Intercambia recursivamente en la grilla para simular la gravedad
gravity(Grid,[],_NoC,Grid):- !.
gravity(Grid,[X|Xs],NoC,RGrid) :-
	NI is X-NoC,
	recursive_swap(Grid,X,NI,NoC,RRGrid),
	gravity(RRGrid,Xs,NoC,RGrid),!.

% generate(+List,+Index,-RList)
% Genera nuevos bloques en las posiciones vacias de la grilla
generate([],40,[]).
generate([0|T],I,[NewNumber|RGrid]) :-
	NI is I+1,
	random(1,8,Exp),
	NewNumber is 2**Exp,
	generate(T,NI,RGrid).
generate([H|T],I,[H|RGrid]) :-
	NI is I+1,
	generate(T,NI,RGrid).

% equal(+Grid,+NI,+I)
% Compara los valores a partir de su indice en la grilla
equal(Grid, NI, I):-
	nth0(NI, Grid, V),
	nth0(I, Grid, Z),
	V is Z.

% not_member(+X,+List)
% Determina si X es un elemento de la lista dada
not_member(_, []).
not_member(X, [H|T]) :-
     X \= H,
    not_member(X, T).

% check_up(+I,-List)
% En caso de no poder movernos hacia arriba, devuelve los valores que 
%no tendremos que considerar al momento de calcular las adyacencias
check_up(I,NoC,[-6,-5,-4]) :- I < NoC,!.
check_up(I,NoC,[]) :- 		  I > NoC-1.

% check_down(+I,-List)
% En caso de no poder movernos hacia abajo, devuelve los valores que 
%no tendremos que considerar al momento de calcular las adyacencias
check_down(I,NoC,[]):-          I < NoC*7,!.
check_down(I,NoC,[4,5,6]):-  	I > (NoC*7)-1.

% check_left(+I,-List)
% En caso de no poder movernos hacia la izquierda, devuelve los valores que 
%no tendremos que considerar al momento de calcular las adyacencias
check_left(I,NoC,[]):-        Mod is I mod NoC, Mod =\= 0,!.
check_left(I,NoC,[-6,-1,4]):- Mod is I mod NoC, Mod is 0.

% check_right(+I,-List)
% En caso de no poder movernos hacia la derecha, devuelve los valores que 
%no tendremos que considerar al momento de calcular las adyacencias
check_right(I,NoC,[]):-       Mod is I mod NoC, Mod =\= NoC-1,!.
check_right(I,NoC,[-4,1,6]):- Mod is I mod NoC, Mod is NoC-1.

% valid_moves(+Index,+NoC,-List)
% Calcula las posiciones hacia las que nos podemos mover dado un determinado indice
valid_moves(Index,NoC,List) :-
	AllList = [-6,-5,-4,-1,1,4,5,6],
	check_up(Index,NoC,LU),    
	check_down(Index,NoC,LD),  
	check_right(Index,NoC,LR), 
	check_left(Index,NoC,LL),  
	union(LU,LD,Aux),
	union(Aux,LR,Aux2),
	union(Aux2,LL,NList),
	subtract(AllList,NList,List).

% recursive_find(+Grid,+Ady,+NoC,+Visited,+NAdy,-ReturnAdy)
% Busca recursivamente adyacencias en aquellos bloques que ya fueron 
% identificados como adyacentes válidos
recursive_find(_Grid,[],_NoC,_Visited,NAdy, NAdy).
recursive_find(Grid,[X|Xs],NoC,Visited,NAdy, ReturnAdy):-
	valid_moves(X,NoC,NPossible),
	find_adyacencies(Grid,X,NoC,NAdy,NPossible,Visited,RAdy),
    union(NAdy,RAdy,NewAdy),
	recursive_find(Grid,Xs,NoC,Visited,NewAdy, ReturnAdy).

% add_index(+Ady,+Index,+AuxAdy,-Moves)
% Añade el indice actual a la lista de adyacencia en caso de no estar presente en la misma
add_index(Ady,_,[],Ady).
add_index(Ady,Index,_AuxAdy,Moves) :-
    union(Ady,[Index],Moves).

% merge_adys(+Grid,+NoC,+Moves,+AuxAdy,+Visited,-ReturnAdy)
% Buscamos nuevas adyacencias de forma recursiva
merge_adys(_,_NoC,Moves,[],_Visited,Moves).
merge_adys(Grid,NoC,Moves,AuxAdy,Visited,ReturnAdy) :-
    union(Moves,AuxAdy,RAdy),
	recursive_find(Grid,AuxAdy,NoC,Visited,RAdy,ReturnAdy).

% find_adyacencies(+Grid,+Index,+NoC,+Ady,+Possible,+Visited,-RAdy)
% Genera una lista con los indices adyacentes al indice dado y que a su vez comparten valor con este
% - Grid: La grilla del juego
% - Index: El índice sobre el cual se buscaran adyacencias válidas
% - Ady: La lista de adyacencia local
% - Possible: La lista con las jugadas válidas para el indice actual
% - Visited: La lista con todos los elementos visitados hasta el momento
% - RAdy: La nueva lista de adyacencia local
find_adyacencies(Grid,Index,NoC,Ady,Possible,Visited,RAdy) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal(Grid,NI,Index), 
				not_member(NI,Ady),
                not_member(NI,Visited)),AuxAdy),
    add_index(Ady,Index,AuxAdy,Moves),
	merge_adys(Grid,NoC,Moves,AuxAdy,Visited,RAdy ).

% list_booster(+Grid,+GridConsume,+NoC,+Index,+Ady,+Visited,-LoL)
% Genera una lista conformada por listas de adyacencias cuyos indices comparten valor
list_booster(_Grid,[],_NoC,_I,_Ady,_Visited,[]).
list_booster(Grid,[_X|Xs],NoC,I,Ady,Visited,[RAdy|LoL]) :-
	valid_moves(I,NoC,Possible),
	find_adyacencies(Grid,I,NoC,Ady,Possible,Visited,RAdy),
	union(RAdy,Visited,VisitedAUX),
	NI is I+1,
	list_booster(Grid,Xs,NoC,NI,[],VisitedAUX,LoL).

% make_move_booster(+Grid,+Path,-RGrid,-SumaTotal)
% Dada una lista de indices de longitud N, inserta valores vacios en las 
% primeras N-1 posiciones, y genera un bloque con el resultado pertinente en la posicion N 
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

% join_booster(+Grid,+NList,+NumOfColumns,-RGrids)
% Realiza los movimientos, gravedad y generación para el booster a partir de la lista de listas
join_booster(Grid, [], NList, NumOfColumns, RGrids):-
	gravity(Grid,NList,NumOfColumns,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [Grid,RGravity,RGenerate].
join_booster(Grid, [X|Xs], NList, NumOfColumns, RGrids):-
	make_move_booster(Grid, X, Aux, 0),
	create_list_zeros(Aux,0,NNList),
	union(NList,NNList,List),
	join_booster(Aux, Xs,List, NumOfColumns, RGrids).

% booster(+Grid,+NumOfColumns,-RGrid)
% Realiza toda la logica detras del booster "Colapsar Iguales"
booster(Grid, NumOfColumns, RGrid):-
	list_booster(Grid,Grid,NumOfColumns,0,[],[],LoL),
	join_booster(Grid, LoL, [],NumOfColumns, RGrid).

% ------------------------------------------------------------------------------------

recursive_find_maxmove(_Grid,[],_NoC,NAdy,NAdy, 1).
recursive_find_maxmove(_Grid,[],_NoC,_,[], 0).
recursive_find_maxmove(Grid,[X|Xs],NoC,NAdy,[RAdy|L],_):-
	valid_moves(X,NoC,NPossible),
	find_adyacencies_maxmove(Grid,X,NoC,NAdy,NPossible,RAdy,1),
	!,
    recursive_find_maxmove(Grid,Xs,NoC,NAdy,L,0).

equal_or_next(Grid,NI,I):-
    equal(Grid,NI,I).
equal_or_next(Grid,NI,I):-
    nth0(NI, Grid, V),
	nth0(I, Grid, Z),
    V is Z*2.

find_adyacencies_maxmove(Grid,Index,NoC,Ady,Possible,RAdy,1) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal_or_next(Grid,NI,Index), 
				not_member(NI,Ady)),AuxAdy),
    union(Ady,[Index],Moves),
	recursive_find_maxmove(Grid,AuxAdy,NoC,Moves,RAdy,1).

find_adyacencies_maxmove(Grid,Index,NoC,Ady,Possible,RAdy,0) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal(Grid,NI,Index), 
				not_member(NI,Ady)),AuxAdy),
    union(Ady,[Index],Moves),
	recursive_find_maxmove(Grid,AuxAdy,NoC,Moves,RAdy,1).

list_maxmove(_Grid,[],_NoC,_I,_Ady,[]).
list_maxmove(Grid,[_X|Xs],NoC,I,Ady,[RAdy|LoL]) :-
	valid_moves(I,NoC,Possible),
	find_adyacencies_maxmove(Grid,I,NoC,Ady,Possible,RAdy,0),
	NI is I+1,
	list_maxmove(Grid,Xs,NoC,NI,[],LoL).

make_move_maxmove(_Grid,[],SumaTotal,SumaTotal).
make_move_maxmove(Grid,[X|Xs],SumaTotal,Return):-
	nth0(X,Grid,Valor),
	SumaTotalAux is SumaTotal + Valor,
	make_move_maxmove(Grid,Xs,SumaTotalAux,Return).
	

get_maxmove(_,[],MaxList,MaxList,MaxValue,MaxValue).
get_maxmove(Grid,[X|Xs],_,RList,MaxValue,RValue):-
    \+number(X),
	make_move_maxmove(Grid,X,0,Value),
	Value > MaxValue,
	get_maxmove(Grid,Xs,X,RList,Value,RValue).
get_maxmove(Grid,[_|Xs],MaxList,RList,MaxValue,RValue):-
	get_maxmove(Grid,Xs,MaxList,RList,MaxValue,RValue).

cleanList([],Aux,Aux).
cleanList([[X|Xs]|L],Aux,CCl):-
    cleanList([X|Xs],Aux,Cl),
    cleanList(L,Cl,CCl).
cleanList([X|Xs],Aux,CCl):-
    cleanList(X,Aux,Cl),
    cleanList(Xs,Cl,CCl).
cleanList([X],Aux,[X|Aux]).
cleanList([X|Xs],Aux,[[X|Xs]|Aux]).

format_maxmove([],[],_NoC).
format_maxmove([I|Is],[[PosX|[PosY]]|Ls],NoC) :-
	PosY is I mod NoC,
	PosX is (I-PosY) / NoC,
	format_maxmove(Is,Ls,NoC).

maxmove(Grid,RList,NoC):-
	list_maxmove(Grid,Grid,NoC,0,[],LoL),
    cleanList(LoL,[],Cl),
	get_maxmove(Grid,Cl,_,MaxList,0,_),
	format_maxmove(MaxList,RList,NoC).

recursive_find_maxAd(_Grid,[],_NoC,NAdy,NAdy).
recursive_find_maxAd(Grid,[X|Xs],NoC,NAdy,[NewAdy|L]):-
	valid_moves(X,NoC,NPossible),
	find_adyacencies_maxAd(Grid,X,NoC,NAdy,NPossible,RAdy,1),
    union(NAdy,RAdy,NewAdy),
    recursive_find_maxAd(Grid,Xs,NoC,NAdy,L).

find_adyacencies_maxAd(Grid,Index,NoC,Ady,Possible,RAdy,1) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal_or_next(Grid,NI,Index), 
				not_member(NI,Ady)),AuxAdy),
    union(Ady,[Index],Moves),
	recursive_find_maxAd(Grid,AuxAdy,NoC,Moves,RAdy).

find_adyacencies_maxAd(Grid,Index,NoC,Ady,Possible,RAdy,0) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal(Grid,NI,Index), 
				not_member(NI,Ady)),AuxAdy),
    union(Ady,[Index],Moves),
	recursive_find_maxAd(Grid,AuxAdy,NoC,Moves,RAdy).

list_maxAd(_Grid,[],_NoC,_I,_Ady,[]).
list_maxAd(Grid,[_X|Xs],NoC,I,Ady,[RAdy|LoL]) :-
	valid_moves(I,NoC,Possible),
	find_adyacencies_maxAd(Grid,I,NoC,Ady,Possible,RAdy,0),
	NI is I+1,
	list_maxAd(Grid,Xs,NoC,NI,[],LoL).

make_move_maxAd(Grid,[],Grid,SumaTotal,SumaTotal):- !.
make_move_maxAd(Grid,[P],RGrid,SumaTotal,R) :-
	nth0(P,Grid,Value),
	SumaTotalAux is SumaTotal + Value,
	result(SumaTotalAux, 1, Bloque),
	replace(Grid, P, Bloque, Aux),
	make_move_maxAd(Aux,[],RGrid,Bloque,R),
    !.
make_move_maxAd(Grid,[P|Ps],RGrid,SumaTotal,R):-
	nth0(P,Grid,Value),
	replace(Grid, P, 0, Aux),
	SumaTotalAux is SumaTotal + Value,
	make_move_maxAd(Aux,Ps,RGrid,SumaTotalAux,R).

checkAdy(Grid,NoC,Index):-
	valid_moves(Index,NoC,Possible),
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal(Grid,NI,Index)),AuxAdy),
    \+length(AuxAdy,0).

traceLast(Grid,_,Lx,VLx,Lx):-
	nth0(Lx,Grid,VLx),!.
traceLast(Grid,NoC,Lx,VLx,Rx):-
	NLx is Lx+NoC,
	traceLast(Grid,NoC,NLx,VLx,Rx).

get_maxAd(_,_,[],_MaxList,[],0,0).
get_maxAd(_,_,[],MaxList,MaxList,MaxValue,MaxValue).
get_maxAd(Grid,NoC,[X|Xs],_,RList,MaxValue,RValue):-
    \+number(X),
	make_move_maxAd(Grid,X,RGrid,0,Value),
	Value > MaxValue,
	create_list_zeros(RGrid,0,NList),
	last(X,Lx),
	nth0(Lx,RGrid,VLx),
	gravity(RGrid,NList,NoC,RGravity),
	traceLast(RGravity,NoC,Lx,VLx,Rx),
    checkAdy(RGravity,NoC,Rx),
    get_maxAd(Grid,NoC,Xs,X,RList,Value,RValue).
get_maxAd(Grid,NoC,[_|Xs],MaxList,RList,MaxValue,RValue):-
	get_maxAd(Grid,NoC,Xs,MaxList,RList,MaxValue,RValue).

maxAd(Grid,NoC,RList):-
    list_maxAd(Grid,Grid,NoC,0,[],LoL),
    cleanList(LoL,[],Cl),
    get_maxAd(Grid,NoC,Cl,_,MaxList,0,_MaxValue),
	format_maxmove(MaxList,RList,NoC).

% ------------------------------------------------------------------------------------


% join(+Grid,+NumOfColumns,+Path,-RGrids)
% Realiza toda la logica detras de una jugada
join(Grid, NumOfColumns, Path, RGrids):-
	make_move(Grid,Path,NumOfColumns,RMove,0),
	create_list_zeros(RMove,0,NList),
	gravity(RMove,NList,NumOfColumns,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [RMove,RGravity,RGenerate].