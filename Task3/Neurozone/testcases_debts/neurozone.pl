read_and_return(F, N, M, Debts) :-
    !,open(F, read, Stream),
    read_line_to_codes(Stream, FirstLine),
    atom_codes(A, FirstLine),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, [N, M]),
    read_debts(Stream, M, [], Pairs),
    keysort(Pairs, SortedPairs),
    group_pairs_by_key(SortedPairs,Debts),
    close(Stream).

read_debts(_, 0, Acc, Acc).
read_debts(Stream, M, Acc, Debts) :-
	M > 0,
        read_line_to_codes(Stream, Line),
        atom_codes(A, Line),
        atomic_list_concat(As, ' ', A),
        maplist(atom_number, As, [D, T1, X1, T2, X2]),
	ND is -D,
	D1 = T2-(X2,D),
	D2 = T1-(X1,ND),
        M1 is M - 1,
	Acc1 = [D1,D2|Acc],
        read_debts(Stream, M1, Acc1, Debts).

halve([], [], []).
halve([A], [A], []).
halve([A,B|Cs], [A|X], [B|Y]) :- halve(Cs, X, Y).

merge([], Ys, Ys).
merge(Xs, [], Xs).
merge([(X1,X2)|Xs], [(Y1,Y2)|Ys], M) :-
   ( X1 < Y1 -> M = [(X1,X2)|Ms], merge(Xs, [(Y1,Y2)|Ys], Ms) ; M = [(Y1,Y2)|Ms], merge([(X1,X2)|Xs], Ys, Ms) ).

mergeSort([], []).
mergeSort([E], [E]).
mergeSort([E1,E2|Es], SL) :- halve([E1,E2|Es], L1, L2), mergeSort(L1, SL1), mergeSort(L2, SL2), merge(SL1, SL2, SL).

timesort([],[]).
timesort([Key-Pairs|T],[X|Xs]):-
	mergeSort(Pairs,SortedPairs),
	X = Key-SortedPairs,
	timesort(T,Xs). 

timesumAux(_, _, [], Acc, Acc2):- reverse(Acc,Acc2).
timesumAux(K, D, [(Time,Debts)], Acc, Result):-	
	(K =:= Time ->
	    D1 is D+Debts,
	    X = (K,D1),
	    Acc1 = [X|Acc],
	    timesumAux(K, D1, [], Acc1, Result)
	;X1 = (K, D),
	 X2 = (Time,Debts),
	 Acc1 = [X2,X1|Acc],
	 timesumAux(K, D, [], Acc1, Result)).
timesumAux(K,D,[(Time,Debts)|T], Acc, Result):-
	(K =:= Time ->
	    D1 is D+Debts,
	    timesumAux(K, D1, T, Acc, Result)
	;X = (K,D),
	 Acc1 = [X|Acc],
	 timesumAux(Time, Debts, T, Acc1, Result)).

timesum([],[]).
timesum([_-[]|T],[X|Xs]):-
	X = [],	
	timesum(T,Xs).
timesum([_-[(Time,Debts)|T1]|T],[X|Xs]):-
	timesumAux(Time, 0, [(Time,Debts)|T1], [], X),
        timesum(T,Xs). 

fillKeys([], _, Acc, Acc).
fillKeys([Key-Pairs|T], Count, Acc, Result):-
	NCount is Count - 1,
	(Key =:= Count ->
		Acc1 = [Key-Pairs|Acc],
		fillKeys(T, NCount, Acc1, Result)
	; Acc1 = [Count-[]|Acc],
	  fillKeys([Key-Pairs|T], NCount, Acc1, Result)).

writeListaux(Stream, []):- close(Stream).
writeListaux(Stream, [H|T]):-
	write(Stream,H),
	write(Stream, ' '),
	writeListaux(Stream,T).
	
writeList(F,L):-
	open(F,write,Stream),
	writeListaux(Stream,L).
	
balance([], _, Acc, Acc).
balance([(_,Debts)|T], Sum, Acc, Result):-
	S1 is Sum + Debts,
	(S1 < 0 ->
	    Acc1 is Acc - S1,
	    balance(T, 0, Acc1, Result)
  	;balance(T, S1, Acc, Result)).

give_funds([],[]).
give_funds([PairsList|T],[X|Xs]):-
	balance(PairsList,0,0,X),
	give_funds(T,Xs).
    		
neurozone(F,Solution):-
	read_and_return(F, N, M,D),
	once(timesort(D,D1)),
	reverse(D1,D2),
	once(fillKeys(D2,N,[],D3)),
    	once(timesum(D3,Debts)),
        give_funds(Debts, Solution).
