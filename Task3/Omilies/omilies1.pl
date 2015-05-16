omilies(F, Answers):-
	once(read_and_return(F, Stream, N, Graph, CityA, CityB, CityC, L)),
	once(initialize(N, CityA, [], DistA)),
    once(initialize(N, CityB, [], DistB)),
    once(initialize(N, CityC, [], DistC)),
    once(dijkstra([(0,CityA)], Graph, DistA, AList)),
    once(dijkstra([(0,CityB)], Graph, DistB, BList)),
    once(dijkstra([(0,CityC)], Graph, DistC, CList)),
	questions(Stream, AList, BList, CList, L, Answers),
	close(Stream).



read_and_return(F, Stream, N, Graph, CityA, CityB, CityC, L):-
	!,open(F, read, Stream),
	read_line_to_codes(Stream, FirstLine),
   	atom_codes(A, FirstLine),
   	atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, [N, M]),
	read_edges(Stream, M, [], Pairs),
	keysort(Pairs, SortedPairs),
	group_pairs_by_key(SortedPairs,Graph),
	read_line_to_codes(Stream, Line),
	atom_codes(B, Line),
    atomic_list_concat(Bs, ' ', B),
    maplist(atom_number, Bs, [CityA, CityB, CityC, L]).
	
	
read_edges(_,0, Acc, Acc).
read_edges(Stream, M, Acc, Edges):-
		!,M > 0,
        read_line_to_codes(Stream, Line),
        atom_codes(A, Line),
        atomic_list_concat(As, ' ', A),
        maplist(atom_number, As, [City1,City2,Dist]),
        E1 = City1-(City2,Dist),
	    E2 = City2-(City1,Dist),
        M1 is M - 1,
	    Acc1 = [E1,E2|Acc],
        read_edges(Stream, M1, Acc1, Edges).
	
	

my_neighbours(_,[],[]).
my_neighbours(K,[Key-List|T],X):-
	!,(K =:= Key -> 
		X = List
	;my_neighbours(K,T,X)).


/*update_element(0,PQueue,PQueue,Rest,Rest,_,_).
update_element(K,PQueue,Result,[(A,B)|T],[X|Xs],Dist,Pos):-
	!,K1 is K-1,
	(K > 1 -> 
		X = (A,B),	
		update_element(K1,PQueue,Result,T,Xs,Dist,Pos)
	;(Dist < B ->
	    X = (A,Dist),
		ord_add_element(PQueue,(Dist,Pos),NPQueue),
		update_element(K1,NPQueue,Result,T,Xs,Dist,Pos)
	 ;X = (A,B),
	 update_element(K1,PQueue,Result,T,Xs,Dist,Pos))).*/
	 
	 
update_element(0, PQueue,PQueue,List, _, Acc, Result, _):-
        reverse(Acc,Acc1),
        append(Acc1,List,Result).
update_element(K,PQueue, Acc2,[(A,B)|T], Dist, Acc, Result, Pos):-
        K1 is K-1,
        (K > 1 ->
	   Acc1 = [(A,B)|Acc],
           update_element(K1,PQueue,Acc2,T,Dist,Acc1,Result, Pos)
        ;(Dist < B ->
	      Acc1 = [(A,Dist)|Acc],
		  ord_add_element(PQueue,(Dist,Pos),NPQueue),
		  update_element(K1,NPQueue,Acc2,T,Dist,Acc1,Result,Pos)
	    ;Acc1 = [(A,B)|Acc],
	     update_element(K1,PQueue,Acc2,T,Dist,Acc1,Result,Pos))).


update(PQueue,PQueue,DistList, [], DistList,_).
update(PQueue,Acc,DistList,[(City,Dist)|T], Result, Udist):-
	!,Alt is Udist + Dist,
	once(update_element(City,PQueue,NPQueue,DistList,Alt,[],NewDistList,City)),
	update(NPQueue,Acc,NewDistList, T, Result, Udist).


peekAt(1,[(_,Dist)|_],Dist).
peekAt(N,[_|T],Dist):- 
	!,N1 is N - 1,
	peekAt(N1,T,Dist).

initialize(0, _, Acc, Acc).
initialize(K, City, Acc, DistList):-
	!,K > 0,
	K1 is K-1,
	(City =\= K ->
		Acc1 = [(false,10000000)|Acc]
	;Acc1 = [(false,0)|Acc]),
	initialize(K1, City, Acc1, DistList).
	

dijkstra([],_,DistList,DistList).
dijkstra([(Min,Pos)|T],Graph,DistList,Result):-
	!,once(my_neighbours(Pos,Graph,Neighbours)),
	once(update(T,NPQueue,DistList, Neighbours, NewList, Min)),
	write(ok),nl,
	dijkstra(NPQueue,Graph,NewList,Result).
	
	
questions(Stream, AList, BList, CList, L, Answers):-
	!,(L > 0 ->
		Answers = [Answer|As],
		read_line_to_codes(Stream, Line),
		atom_codes(A, Line),
		atom_number(A, City),
		peekAt(City, AList, ADist),
		peekAt(City, BList, BDist),
		peekAt(City, CList, CDist),
		search(AList,BList,CList,ADist,BDist,CDist,Answer),
		L1 is L-1,
		questions(Stream, AList, BList, CList, L1, As)
	;L =:= 0 ->
		Answers = []).
	
search([], _, _, _, _, _,true).
search([(_,HA)|TA], [(_,HB)|TB], [(_,HC)|TC], ADist, BDist, CDist, Answer):-
	!,(HA < ADist , HB < BDist , HC < CDist ->
		Answer = false
	;search(TA, TB, TC, ADist, BDist, CDist, Answer)).
