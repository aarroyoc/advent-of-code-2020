:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([X|Data]) -->
    integer(X),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Data) :-
    open('input.dat', read, Stream),
    phrase_from_stream(input(Data), Stream).

star(1) :-
    load_data(Numbers),
    member(A, Numbers),
    member(B, Numbers),
    A + B =:= 2020,
    X is A * B,
    write(X).

star(2) :-
    load_data(Numbers),
    member(A, Numbers),
    member(B, Numbers),
    member(C, Numbers),
    A + B + C =:= 2020,
    X is A * B * C,
    write(X).