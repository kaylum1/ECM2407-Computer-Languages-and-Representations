%Question 4.1
% Predicate to check if a number is prime
is_prime(2) :- !.
is_prime(3) :- !.
is_prime(N) :-
    N > 3,
    N mod 2 =\= 0,
    \+ has_factor(N, 3).



% Helper predicate to check if a number has a factor
has_factor(N, Factor) :-
    N mod Factor =:= 0.
has_factor(N, Factor) :-
    Factor * Factor < N,
    NextFactor is Factor + 2,
    has_factor(N, NextFactor).


% Helper predicate to convert a run of digits to a number
run_to_number(R, N) :-
    run_to_number(R, 0, N).

run_to_number([], Acc, Acc).
run_to_number([D | R], Acc, N) :-
    NewAcc is Acc * 10 + D,
    run_to_number(R, NewAcc, N).




generator4(Runs) :-
  maplist(run_to_number, Runs, N),
  maplist(is_prime, N).

x_generator4( N ) :- 
    x_generator4_loop(
        [ [[9 ,6 ,7] ,[4 ,0 ,1] ,[2 ,8 ,3] ,[5]]
        , [[9 ,8 ,3] ,[6 ,0 ,1] ,[5] ,[4 ,7] ,[2]]
        , [[9 ,8 ,3] ,[6 ,7] ,[4 ,2 ,0 ,1] ,[5]]
        , [[9 ,8 ,5 ,1] ,[2] ,[4 ,3] ,[6 ,0 ,7]]
        , [[9 ,8 ,5 ,1] ,[2] ,[3] ,[6 ,0 ,4 ,7]]
        , [[9 ,8 ,5 ,1] ,[2] ,[7] ,[4 ,6 ,0 ,3]]
        , [[8 ,9] ,[7] ,[6 ,0 ,1] ,[2 ,5 ,4 ,3]]
        , [[8 ,9] ,[7] ,[5 ,6 ,3] ,[4 ,0 ,2 ,1]]
        , [[8 ,9] ,[5] ,[4 ,7] ,[6 ,0 ,1] ,[3] ,[2]]
        , [[3],[5],[6,0,7],[2],[4,1],[8,9]] ], 0, N ).

x_generator4_loop( [], C, C ). 
x_generator4_loop( [T|TS], C, N ) :-
    generator4( T ),
    C1 is C + 1,
    x_generator4_loop( TS, C1, N ). 
x_generator4_loop( [_|TS], C, N ) :-
    x_generator4_loop( TS, C, N ).




%QUESTION 4.2 ALL STEPS NEEDING TO GET SOLUTIONS-----------------------------
concatenate_list(List, [N]) :-
 atomic_list_concat(List, '', Atom),
 atom_number(Atom, N).


concatenate_lists(Input, Output) :-
 maplist(concatenate_list, Input, Output).




%predicates used to get rid of the smallest number (prime)
list_min([L|Ls], Min) :- 
    foldl(num_num_min, Ls, L, Min).

num_num_min(X, Y, Min) :- 
   (  X =< Y
   -> Min = X
   ;  Min = Y
   ).

remove_min([], []).
remove_min(List, ListWithoutMin) :-
   list_min(List, Min),
   select(Min, List, ListWithoutMin).




flatten([], []).
flatten([H|T], Result) :-
   is_list(H),
   flatten(H, FlatH),
   flatten(T, FlatT),
   append(FlatH, FlatT, Result).
flatten([H|T], [H|Result]) :-
   flatten(T, Result).

%predicate use dto arramge list in decreasing order
decrease_list(List, SortedList) :-
   sort(0, @>=, List, SortedList).



split_number_in_list([N], Digits) :-
    number_chars(N, CharList),
    maplist(atom_number, CharList, Digits).



get_number(XS, N) :-
    reverse_list(XS, SX), list_to_number(SX, N).

list_to_number([], 0).
list_to_number([H|T], N) :-
    list_to_number(T, N1), 
    N is N1 * 10 + H.


reverse_list([], []).
reverse_list([X|XS], W) :-
    reverse_list(XS, V), 
    append(V, [X], W).


check_cube(N) :-
    check_cube(N, 0).

check_cube(N, R) :-
    S is R * R * R,
    N =:= S.
check_cube(N, R) :-
    S is R * R * R,
    S < N,
    R1 is R + 1,
    check_cube(N, R1).


check_cube_list([]).
check_cube_list([N1, N2, N3, N4|T]) :-
    get_number([N1, N2, N3, N4], N),
    check_cube(N),
    check_cube_list(T).
check_cube_list([N1, N2, N3|T]) :-
    get_number([N1, N2, N3], N),
    check_cube(N),
   	check_cube_list(T).
check_cube_list([N1, N2|T]) :-
    get_number([N1, N2], N),
    check_cube(N),
    check_cube_list(T).
check_cube_list([N1|T]) :-
    get_number([N1], N),
    check_cube(N), 
    check_cube_list(T).



tester4(A) :-
    concatenate_lists(A, B),
   	flatten(B, C),
    remove_min(C, D),
    decrease_list(D, E),
    concatenate_list(E, F),
    split_number_in_list(F, G),
    check_cube_list(G).




x_tester4( N ) :- 
    x_tester4_loop(
        [ [[8 ,2 ,7] ,[6 ,1] ,[5 ,3] ,[4 ,0 ,9]] 
        , [[8 ,2 ,7] ,[6 ,1] ,[4 ,0 ,9] ,[5 ,3]]
        , [[8 ,2 ,7] ,[5 ,3] ,[6 ,1] ,[4 ,0 ,9]]
        , [[8 ,2 ,7] ,[4 ,0 ,9] ,[6 ,1] ,[5 ,3]]
        , [[6 ,1] ,[8 ,2 ,7] ,[4 ,0 ,9] ,[5 ,3]]
        , [[6 ,1] ,[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7]]
        , [[5 ,3] ,[6 ,1] ,[4 ,0 ,9] ,[8 ,2 ,7]]
        , [[5 ,3] ,[4 ,0 ,9] ,[6 ,1] ,[8 ,2 ,7]]
        , [[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7] ,[6 ,1]]
        , [[4,0,9],[8,2,7],[6,1],[5,3]] ], 0, N ).

x_tester4_loop( [], C, C ). 
x_tester4_loop( [T|TS], C, N ) :-
    tester4( T ),
    C1 is C + 1,
    x_tester4_loop( TS, C1, N ).
x_tester4_loop( [_|TS], C, N ) :- 
    x_tester4_loop( TS, C, N ).




main :-
%x_generator4( N ), write(N).
x_tester4( N ), write(N).
%generator4( XS ), tester4( XS ), write( XS ).
