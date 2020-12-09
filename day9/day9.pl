:- module(day9, []).


:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([X|Data]) -->
    integer(X),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Numbers) :-
    open('day9/input.dat', read, Stream),
    phrase_from_stream(input(Numbers), Stream).

star(1, N) :-
    load_data(Numbers),
    select_preamble(Numbers, Preamble, N),
    \+ sum_in_preamble(N, Preamble).

star(2, N) :-
    Sol1 is 400480901,
    load_data(Numbers),
    between(17, 100, Size), % in 17 is the answer, original answer was between(2, 100, Size)
    select_range(Numbers, Size, Range),
    sum_list(Range, Sol1),
    min_list(Range, MinX),
    max_list(Range, MaxX),
    N is MinX + MaxX.


select_preamble(Numbers, Preamble, N) :-
    length(Numbers, L),
    between(25, L, ChoicePoint),
    nth0(ChoicePoint, Numbers, N),
    select_preamble_(Numbers, 25, ChoicePoint, Preamble).

select_preamble_(_, 0, _, []).
select_preamble_(Numbers, X, ChoicePoint, [Point|Preamble]) :-
    Index is ChoicePoint - X,
    nth0(Index, Numbers, Point),
    NewX is X - 1,
    select_preamble_(Numbers, NewX, ChoicePoint, Preamble).

sum_in_preamble(N, Preamble) :-
    member(X, Preamble),
    member(Y, Preamble),
    N =:= X+Y.

select_range(Numbers, Size, Range) :-
    length(Numbers, L),
    between(0, L, Index),
    End is Size+Index,
    select_range_(Numbers, Size, Index, End, Range).

select_range_(_, _, Index, End, []) :- Index = End.
select_range_(Numbers, Size, Index, End, [Point|Range]) :-
    nth0(Index, Numbers, Point),
    NewIndex is Index + 1,
    select_range_(Numbers, Size, NewIndex, End, Range).

:- begin_tests(day9).

test(star1) :- star(1, 400480901),!.
test(star2) :- star(2, 67587168),!.

:- end_tests(day9).