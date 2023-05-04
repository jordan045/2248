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

equal(Grid, I, X):-
	nth0(I, Grid, V),
	V is X.

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

check_adyacent(_,[]).
check_adyacent(I, [X|Xs]):-
	\+ X is I,
	check_adyacent(I,Xs).

/*chequearLoL(I,[]).
chequearLoL(I,[[X|H]|T]):-
	chequearLoL(I,[X|H]).
chequearLoL(I,[X|H]):-
	check_adyacent(I,[X|H]).*/

create_list_booster([],40,[],_).	
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_up(I),
	check_left(I),
	X > 0,
	NI is I-6,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_up(I),
	X > 0,
	NI is I-5,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_right(I),
	check_up(I),
	X > 0,
	NI is I-4,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_left(I),
	X > 0,
	NI is I-1,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_right(I),
	X > 0,
	NI is I+1,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_down(I),
	check_left(I),
	X > 0,
	NI is I+4,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_down(I),
	X > 0,
	NI is I+5,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([X|Xs],I,[[I|H]|T],Aux):-
	check_down(I),
	check_right(I),
	X > 0,
	NI is I+6,
	equal([X|Xs],NI,X),
	%chequearLoL(I,[[H]|T]),
	check_adyacent(NI, Aux),
	create_list_booster(Xs, NI, [[H]|T],[I|Aux]).
create_list_booster([_|Xs],I,T,_) :-
	NI is I+1,
	create_list_booster(Xs,NI,T,_).

join_booster(Grid, [], NList, RGrids):-
	gravity(Grid,NList,RGravity),
	generate(RGravity,0,RGenerate),
	RGrids = [Grid,RGravity,RGenerate].
join_booster(Grid, [X|Xs], NList, RGrids):-
	set_all_cero(Grid, X, Aux, 0),
	create_list_zeros(Aux,0,NList),
	join_booster(Aux, Xs,NList, RGrids).

booster(Grid, RGrid):-
	create_list_booster(Grid, 0, LoL,_),
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