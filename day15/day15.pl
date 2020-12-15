:- module(day15, []).

:- use_module(library(assoc)).
:- use_module(library(hashtable)).

star(1, N) :-
    list_to_assoc([6-1,3-2,15-3,13-4,1-5], Store),
    find_position(7, 2020, 0, Store, N).

star(2, N) :-
    ht_pairs(Store, [6-1,3-2,15-3,13-4,1-5]),
    find_position_ht(7, 30000000, 0, Store, N).

find_position(N, To, Last, Store, X) :-
    OldN is N - 1,
    (get_assoc(Last, Store, Pos) ->
        (
            NewLast is OldN - Pos,
            put_assoc(Last, Store, OldN, NewStore)
        );(
            NewLast is 0,
            put_assoc(Last, Store, OldN, NewStore)
        )
    ),
    NewN is N + 1,
    (NewN > To ->
        NewLast = X
    ;   find_position(NewN, To, NewLast, NewStore, X)).

find_position_ht(N, To, Last, Store, X) :-
    OldN is N - 1,
    (ht_get(Store, Last, Pos) ->
        (
            NewLast is OldN - Pos,
            ht_put(Store, Last, OldN)
        );(
            NewLast is 0,
            ht_put(Store, Last, OldN)
        )
    ),
    NewN is N + 1,
    (NewN > To ->
        NewLast = X
    ;   find_position_ht(NewN, To, NewLast, Store, X)).


:- begin_tests(day15).

test(star1) :- star(1, 700),!.
test(star2) :- star(2, 51358),!.

:- end_tests(day15).

