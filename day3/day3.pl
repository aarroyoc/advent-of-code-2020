:- module(day3, []).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([X|Data]) -->
    string(X),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Map) :-
    open('day3/input.dat', read, Stream),
    phrase_from_stream(input(Lines), Stream),
    maplist(string_chars, Lines, Map).

star(1, N) :-
    load_data(Map),
    steps(0, 0, Map, N).

star(2, N) :-
    load_data(Map),
    steps2(0, 0, 1, 1, Map, N1),
    steps2(0, 0, 3, 1, Map, N2),
    steps2(0, 0, 5, 1, Map, N3),
    steps2(0, 0, 7, 1, Map, N4),
    steps2(0, 0, 1, 2, Map, N5),
    N is N1 * N2 * N3 * N4 * N5.

steps(X, Y, Map, N) :-
    NewY is Y + 1,
    nth0(NewY, Map, Line),
    length(Line, LineLength),
    NewX is (X + 3) mod LineLength,
    nth0(NewX, Line, '.'),
    steps(NewX, NewY, Map, N).

steps(X, Y, Map, N) :-
    NewY is Y + 1,
    nth0(NewY, Map, Line),
    length(Line, LineLength),
    NewX is (X + 3) mod LineLength,
    nth0(NewX, Line, '#'),
    steps(NewX, NewY, Map, N1),
    N is N1 + 1.

steps(_, _, _, 0).

steps2(X, Y, SlopeX, SlopeY, Map, N) :-
    NewY is Y + SlopeY,
    nth0(NewY, Map, Line),
    length(Line, LineLength),
    NewX is (X + SlopeX) mod LineLength,
    nth0(NewX, Line, '.'),
    steps2(NewX, NewY, SlopeX, SlopeY, Map, N).

steps2(X, Y, SlopeX, SlopeY, Map, N) :-
    NewY is Y + SlopeY,
    nth0(NewY, Map, Line),
    length(Line, LineLength),
    NewX is (X + SlopeX) mod LineLength,
    nth0(NewX, Line, '#'),
    steps2(NewX, NewY, SlopeX, SlopeY, Map, N1),
    N is N1 + 1.

steps2(_, _, _, _, _, 0).


:- begin_tests(day3).

test(star1) :- star(1, 162), !.
test(star2) :- star(2, 3064612320), !.

:- end_tests(day3).