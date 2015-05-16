/*Predicate for reading the input file and creating a list
holding the debts of the form [Bank1-(Time1,Debt1),Bank2-(Time2,Debt2),Bank1-(Time3,Debt3),...].
Then it sorts the list in increasing bank order and it groups together
debts that refer to the same bank*/
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

/*Auxiliary predicate for reading the debts of the form (P,T1,X1,T2,X2). When a bank
owns another bank then we put a term (X1,-P) for this bank. If somebody else 
owns the bank we put a term (X2,P).*/
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


/*Predicate for sorting each banks debts in increasing time order*/
timesort([],[]).
timesort([Key-Pairs|T],[X|Xs]):-
	mergeSort(Pairs,SortedPairs),
	X = Key-SortedPairs,
	timesort(T,Xs). 

/*Predicate for suming together all debts refering to
a certain time for one bank.
Example: Bank1 has these debts [(1,2),(1,-1),(2,4),(3,5),(4,-7),(4,5),(5,2)]
The result wil be [(1,1),(2,4),(3,5),(4,-2),(5,2)]*/
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

/*Predicate for suming together all debts refering to
a certain time for each of the banks*/
timesum([],[]).
timesum([_-[]|T],[X|Xs]):-
	X = [],	
	timesum(T,Xs).
timesum([_-[(Time,Debts)|T1]|T],[X|Xs]):-
	timesumAux(Time, 0, [(Time,Debts)|T1], [], X),
        timesum(T,Xs). 

/*If a bank doesn't have any debts or other banks don't
own it then this bank's key isn't in the list created.
This predicate finds missing keys and adds them
along with an empty list of debts.*/
fillKeys([], _, Acc, Acc).
fillKeys([Key-Pairs|T], Count, Acc, Result):-
	NCount is Count - 1,
	(Key =:= Count ->
		Acc1 = [Key-Pairs|Acc],
		fillKeys(T, NCount, Acc1, Result)
	; Acc1 = [Count-[]|Acc],
	  fillKeys([Key-Pairs|T], NCount, Acc1, Result)).


/*Predicate for determing how much money should a bank have
so it can pay it's debts. We find the sum of all debts across time.
If the sum in some point is negative we add the opposite value
in an accumulator so the sum can come back to zero. If the sum 
is positive we simply continue. When the list of debts is empty 
the accumulator holds the initial fund the bank should have*/	
balance([], _, Acc, Acc).
balance([(_,Debts)|T], Sum, Acc, Result):-
	S1 is Sum + Debts,
	(S1 < 0 ->
	    Acc1 is Acc - S1,
	    balance(T, 0, Acc1, Result)
  	;balance(T, S1, Acc, Result)).

/*Predicate for finding the initial fund for each bank.
It uses the auxiliary predicate balance/4*/
give_funds([],[]).
give_funds([PairsList|T],[X|Xs]):-
	balance(PairsList,0,0,X),
	give_funds(T,Xs).
    		
/*Main predicate for the solution. It forms the list of
debts and processes it so it can be in the desired form.
It then calculates the initial funds and put them in the
list Solution.*/
neurozone(F,Solution):-
	read_and_return(F, N, M,D),
	once(timesort(D,D1)),
	reverse(D1,D2),
	once(fillKeys(D2,N,[],D3)),
    	once(timesum(D3,Debts)),
        give_funds(Debts, Solution).
