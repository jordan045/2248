recursive_find_maxmove(_Grid,[],_Visited,NAdy, NAdy).
recursive_find_maxmove(Grid,[X|Xs],Visited,NAdy,ReturnAdy):-
	valid_moves(X,NPossible),
	find_adyacencies_maxmove(Grid,X,NAdy,NPossible,Visited,RAdy,1),
    union(NAdy,RAdy,NewAdy),
	recursive_find_maxmove(Grid,Xs,Visited,NewAdy,ReturnAdy).

merge_adys_maxmove(_,Moves,[],_Visited,Moves).
merge_adys_maxmove(Grid,Moves,AuxAdy,Visited,ReturnAdy) :-
    union(Moves,AuxAdy,RAdy),
	recursive_find_maxmove(Grid,AuxAdy,Visited,RAdy,ReturnAdy).

equal_or_next(Grid,NI,I):-
    equal(Grid,NI,I).
equal_or_next(Grid,NI,I):-
    nth0(NI, Grid, V),
	nth0(I, Grid, Z),
    V is Z*2.

find_adyacencies_maxmove(Grid,Index,Ady,Possible,Visited,RAdy,1) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal_or_next(Grid,NI,Index), 
				not_member(NI,Ady),
                not_member(NI,Visited)),AuxAdy),
    add_index(Ady,Index,AuxAdy,Moves),
	merge_adys_maxmove(Grid,Moves,AuxAdy,Visited,RAdy ).

find_adyacencies_maxmove(Grid,Index,Ady,Possible,Visited,RAdy,0) :-
	findall(NI,(member(X,Possible),			
				NI is X+Index, 
				equal(Grid,NI,Index), 
				not_member(NI,Ady),
                not_member(NI,Visited)),AuxAdy),
    add_index(Ady,Index,AuxAdy,Moves),
	merge_adys_maxmove(Grid,Moves,AuxAdy,Visited,RAdy).

list_maxmove(_Grid,[],_I,_Ady,_Visited,[]).
list_maxmove(Grid,[_X|Xs],I,Ady,Visited,[RAdy|LoL]) :-
	valid_moves(I,Possible),
	find_adyacencies_maxmove(Grid,I,Ady,Possible,Visited,RAdy,0),
	union(RAdy,Visited,VisitedAUX),
	NI is I+1,
	list_maxmove(Grid,Xs,NI,[],VisitedAUX,LoL).