read_and_return(F,Voters):-
	open(F, read, Stream),
        read_line_to_codes(Stream, _),
	read_line_to_codes(Stream, SecondLine),
	atom_codes(B, SecondLine),
	atom_chars(B,Voters),
	close(Stream).
	
/*This predicate checks if the current state is the final one*/	
isWinning([]).
isWinning([w|T]):- !,isWinning(T).
isWinning([e|T]):- !,isWinning1(T).
isWinning([b|T]):- !,isWinning2(T).

isWinning1([]).
isWinning1([e|T]):- !,isWinning1(T).
isWinning1([b|T]):- !,isWinning2(T).

isWinning2([]).
isWinning2([b|T]):- !,isWinning2(T).


/*This predicate excludes from the search states that have 
a certain pattern which makes them  not possible paths to the final state*/
exclude([b|T]):- !,exclude1(T).
exclude([e|T]):- !,exclude(T).
exclude([w|T]):- !,exclude(T).
exclude1([b|T]):- !,exclude2(T).
exclude1([e|T]):- !,exclude(T).
exclude2([b|T]):- !,exclude2(T).
exclude2([w|T]):- !,exclude3(T).
exclude2([e|T]):- !,exclude(T).
exclude3([b|T]):- !,exclude2(T).
exclude3([e|T]):- !,exclude(T).
exclude3([w|_]).
exclude3([w]).


/*Describing all the possible changes that 
can be made in a certain state*/
swap([b,e],[e,b]).
swap([e,w],[w,e]).
swap([b,e,e|T],[e,b,e|T]).
swap([e,b,e|T],[e,e,b|T]).
swap([b,b,e|T],[e,b,b|T]).
swap([b,b,e|T],[b,e,b|T]).
swap([b,w,e|T],[e,w,b|T]).
swap([b,e,w|T],[e,b,w|T]).
swap([b,e,w|T],[b,w,e|T]).
swap([b,e,b|T],[e,b,b|T]).
swap([e,w,w|T],[w,w,e|T]).
swap([e,w,w|T],[w,e,w|T]).
swap([e,e,w|T],[e,w,e|T]).
swap([e,w,e|T],[w,e,e|T]).
swap([e,b,w|T],[w,b,e|T]).
swap([w,e,w|T],[w,w,e|T]).
swap([b,w,e|T],[e,w,b|T]).
swap([w,b,e|T],[w,e,b|T]).


my_append([],L,L).
my_append([H|T],L,Result):- 
	NL = [H|L],
	my_append(T,NL,Result).


/*A predicate which creates all the new
states from a parent one.*/
newStates(X,Front,L):-
	!,newStates(X,Front,[],L).
newStates([A,B],Front,Acc,Acc2):-
	!,findall(X,swap([A,B],X),L1),
	once(mymap(L1,Front,NAcc)),
	append(NAcc,Acc,Acc2).
newStates([A,B,C|T],Front,Acc,Acc2):-
	!,findall(X,swap([A,B,C|T],X),L1),
	once(mymap(L1,Front,L)),
	append(L,Acc,NL),
	NFront = [A|Front],
	newStates([B,C|T],NFront,NL,Acc2).


/*A predicate which appends the front part of the list
to all new born states.Same states are excluded so we 
don't have repeating situations*/
mymap(X,F,L):- empty_nb_set(Set),mymap(X,F,Set,L).
mymap([],_,Acc,Acc2):- nb_set_to_list(Acc,Acc2).
mymap([H|T],Front,Acc,Acc2):-
	!,my_append(Front,H,L1),
	once(add_nb_set(L1,Acc,_)),
	mymap(T,Front,Acc,Acc2).


/*The main predicate of the solution. If the queue to
search is empty then it fails. If we are in state
'1' it means we have found the final state and return 
the result. Otherwise we search the next state in the queue*/
bfsSolver([],_,_,_,_):- !,2 = 3.
bfsSolver(_,_,Acc,Acc,1).
bfsSolver(Queue,Closed,Acc,Acc2,0):-
	bfsSolveraux(Queue,Closed,[],Acc,Acc2).
	

/*Auxiliary predicate for the solution. If the queue
is empty all the states of a certain level are over
so we continue to the next level. Otherwise if the first
element is the winning state bfsSolver is called in 
state '1'. If it's not the winning we check whether 
we have visited the state before and if we can exclude 
it from search. If ti's not visited and not excluded, 
we generate all the new states and we put them in a 
new queue. When the older queue gets empty we swap it 
with the new and so on.*/
bfsSolveraux([],Closed,L,Result,Acc):-
	NResult is Result+1,
	bfsSolver(L,Closed,NResult,Acc,0).
bfsSolveraux([H|T],Closed,L,Result,Acc):-
	(once(isWinning(H)) -> bfsSolver([H|T],Closed,Result,Acc,1)
	;(once(add_nb_set(H,Closed,Bool)),Bool = false -> bfsSolveraux(T,Closed,L,Result,Acc)
	;(exclude(H) -> bfsSolveraux(T,Closed,L,Result,Acc)
	    ;once(newStates(H,[],[],L1)),
	     append(L,L1,NQueue),
	     bfsSolveraux(T,Closed,NQueue,Result,Acc)))).
	
moves(F,Moves):-
	once(read_and_return(F,Voters)),
	once(empty_nb_set(Set)),
	bfsSolver([Voters],Set,0,Moves,0).
