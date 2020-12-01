:- module(day1, []).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([X|Data]) -->
    integer(X),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Data) :-
    open('day1/input.dat', read, Stream),
    phrase_from_stream(input(Data), Stream).

star(1, X) :-
    load_data(Numbers),
    member(A, Numbers),
    member(B, Numbers),
    A + B =:= 2020,
    X is A * B.

star(2, X) :-
    load_data(Numbers),
    member(A, Numbers),
    member(B, Numbers),
    member(C, Numbers),
    A + B + C =:= 2020,
    X is A * B * C.

:- begin_tests(day1).

test(star1) :- star(1, 988771), !.
test(star2) :- star(2, 171933104), !.

:- end_tests(day1).