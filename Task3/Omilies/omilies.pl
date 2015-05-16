/*Main predicate of the solution. It initializes three lists
holding the distances and then uses dijkstra's algorithm to
find the shortest paths from cities A,B,C. Then it creates the
list which is the answer to the problem.*/
omilies(F, Answers):-
	once(read_and_return(F, Stream, N, Graph, CityA, CityB, CityC, L)),
	citySort(Graph, [], SortedGraph),
	reverse(SortedGraph,Graph1),
	once(fillKeys(Graph1,N,[],Graph2)),
	once(initialize(N, CityA, [], DistA)),
        once(initialize(N, CityB, [], DistB)),
        once(initialize(N, CityC, [], DistC)),
        once(dijkstra(N, [(0,CityA)], Graph2, DistA, AList)),
        once(dijkstra(N, [(0,CityB)], Graph2, DistB, BList)),
        once(dijkstra(N, [(0,CityC)], Graph2, DistC, CList)),
	reverse(AList,AList1),
	reverse(BList,BList1),
	reverse(CList,CList1),
	questions(Stream, AList1, BList1, CList1, L, Answers),
	close(Stream).


halve([], [], []).
halve([A], [A], []).
halve([A,B|Cs], [A|X], [B|Y]) :- halve(Cs, X, Y).

merge([], Ys, Ys).
merge(Xs, [], Xs).
merge([(X1,X2)|Xs], [(Y1,Y2)|Ys], M) :-
   ( X1 > Y1 -> M = [(X1,X2)|Ms], merge(Xs, [(Y1,Y2)|Ys], Ms) ; M = [(Y1,Y2)|Ms], merge([(X1,X2)|Xs], Ys, Ms) ).

mergeSort([], []).
mergeSort([E], [E]).
mergeSort([E1,E2|Es], SL) :- halve([E1,E2|Es], L1, L2), mergeSort(L1, SL1), mergeSort(L2, SL2), merge(SL1, SL2, SL).

/*A predicate for sorting the neighbours
of each city in descending order*/
citySort([],Acc,Acc2):- reverse(Acc,Acc2).
citySort([Key-Pairs|T], Acc, Result):-
	mergeSort(Pairs,X),
	Acc1 = [Key-X|Acc],
	citySort(T, Acc1, Result).


/*Predicate for reading the input and building
a graph of the form [City-[(Neighbour1,Dist1),(Neighbour2,Dist2),..]].
The graph is sorted in increasing key order.*/
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
	
/*Predicate for reading the edges of the graph*/	
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
	

/*If a city has no neighbours then that key
will not be in th graph. This predicate fills
in the gaps and for every key that doesn't exist
it adds it along with an empty list of neighbours*/	
fillKeys([], _, Acc, Acc).
fillKeys([Key-Pairs|T], Count, Acc, Result):-
        NCount is Count - 1,
        (Key =:= Count ->
                Acc1 = [Key-Pairs|Acc],
                fillKeys(T, NCount, Acc1, Result)
        ; Acc1 = [Count-[]|Acc],
          fillKeys([Key-Pairs|T], NCount, Acc1, Result)).


/*A predicate which returns the list of
the neighbours of a certain city K*/
my_neighbours(_,[],[]).
my_neighbours(K,[Key-List|T],X):-
	!,(K =:= Key -> 
		X = List
	;my_neighbours(K,T,X)).


/*Predicate to update the distances of cities. It takes
the list of neighbours of the city with the least current 
distance Udist and until it's empty it finds the neighbours
of this city and if their current distance is worse than 
Udist+Dist it updates the distance. For evey city we update 
it's distance we add it in a priority queue so we can search
them in increasing distance order.*/
update(_, PQueue, PQueue, X, [], X, _).
update(N, PQueue, Acc, [A|T1], [(City,Dist)|T2], [X|Xs], Udist):-
	N > 0,
	Alt is Udist + Dist,
	N1 is N-1,
	(N > City ->
	   X = A,
           update(N1,PQueue,Acc,T1,[(City,Dist)|T2],Xs, Udist)
        ;(Alt < A ->
	      X = Alt,
	      ord_add_element(PQueue,(Alt,N),NPQueue),
	      update(N1, NPQueue, Acc, T1, T2, Xs, Udist)
	    ;X = A,
	     update(N1, PQueue, Acc, T1, T2, Xs, Udist))).


/*Predicate for initialization of the list holding
the distances for the dijkstra. It puts a distance of 
10000000 for every city except the one we are building
the dijkstra for. This city takes a 0 distance*/
initialize(0, _, Acc, Acc2):- reverse(Acc,Acc2).
initialize(K, City, Acc, DistList):-
	!,K > 0,
	K1 is K-1,
	(City =\= K ->
		Acc1 = [10000000|Acc]
	;Acc1 = [0|Acc]),
	initialize(K1, City, Acc1, DistList).
	

/*Predicate implementing dijkstra's algorithm.
We take the first element of the priority queue
which tells the min distance and the city having it.
We add this city to a set so we don't search the same
cities. It updates the distances of the neighbours
and then does the same recursively until the priority
queue is empty.*/
dijkstra(N, PQueue, Graph, DistList, Result):-
	empty_nb_set(Visited),
	dijkstra(N, Visited, PQueue, Graph, DistList, Result).
dijkstra(_, _, [], _ , DistList, DistList).
dijkstra(N, Visited, [(Min,Pos)|T],Graph,DistList,Result):-
	(add_nb_set(Pos, Visited,Bool), Bool = true ->
		once(my_neighbours(Pos,Graph,Neighbours)),
		once(update(N, T, NPQueue, DistList, Neighbours, NewList, Min)),
	 	dijkstra(N, Visited, NPQueue,Graph,NewList,Result)
	;dijkstra(N, Visited, T, Graph, DistList, Result)).
	

/*Predicate creating the list of Answers. It reads each of the L
cities, it finds the distances from cities A,B,C and calls
the predicate search.*/
questions(Stream, AList, BList, CList, L, Answers):-
	!,(L > 0 ->
		Answers = [Answer|As],
		read_line_to_codes(Stream, Line),
		atom_codes(A, Line),
		atom_number(A, City),
		nth1(City, AList, ADist),
		nth1(City, BList, BDist),
		nth1(City, CList, CDist),
		search(AList,BList,CList,ADist,BDist,CDist,Answer),
		L1 is L-1,
		questions(Stream, AList, BList, CList, L1, As)
	;L =:= 0 ->
		Answers = []).

/*Auxiliary predicate for determining the answer : true or false.
It takes the distances from cities A,B,C of the city asked
and if it finds a trinity of distances which are all better
the answer is false else it is true.*/	
search([], _, _, _, _, _,true).
search([HA|TA], [HB|TB], [HC|TC], ADist, BDist, CDist, Answer):-
	!,(HA < ADist , HB < BDist , HC < CDist ->
		Answer = false
	;search(TA, TB, TC, ADist, BDist, CDist, Answer)).
