/*
 * This file contains two predicates
 *   - read_and_just_print_codes/1
 *   - read_and_return/4
 * that illustrate reading one line at a time from a file in SWI Prolog.
 * The file has the format of input files of the 'neurozone' exercise.
 */

/*
 * The first predicate is not so interesting. It just shows you how you can
 * write in Prolog a predicate that contains a loop with *only* side-effects.
 * Use as:
 *
 *   ?- read_and_just_print_codes('debts.txt').
 */
read_and_just_print_codes(F) :-
    open(F, read, Stream),
    repeat,
    read_line_to_codes(Stream, X),
    ( X \== end_of_file -> writeln(X), fail ; close(Stream), ! ).

/*
 * The second predicate reads the information of a 'debts.txt' file and
 * returns them in three arguments: two integers and one list of debt/5
 * Prolog structures.  One example is shown below:
 * 
 *   ?- read_and_return('debts.txt', N, M, L).
 *   N = 3,
 *   M = 5,
 *   L = [debt(2, 3, 0, 1, 4), debt(1, 2, 1, 1, 5), debt(2, 2, 5, 3, 8),
 *        debt(3, 1, 6, 2, 7), debt(4, 2, 8, 1, 9)].
 *
 * To read each of the M debts, it uses the auxiliary predicate read_debts/3.
 */

read_and_return(F, N, M, Debts) :-
    open(F, read, Stream),
    read_line_to_codes(Stream, FirstLine),
    atom_codes(A, FirstLine),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, [N, M]),
    read_debts(Stream, M, Debts),
    close(Stream).

read_debts(Stream, M, Debts) :-
    ( M > 0 ->
	Debts = [D|Ds],
        read_line_to_codes(Stream, Line),
        atom_codes(A, Line),
        atomic_list_concat(As, ' ', A),
        maplist(atom_number, As, List),
	D =.. [debt|List],
        M1 is M - 1,
        read_debts(Stream, M1, Ds)
    ; M =:= 0 ->
	Debts = []
    ).
