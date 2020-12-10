:- module(day10, []).

:- use_module(library(tabling)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([X|Data]) -->
    integer(X),
    "\n",
    input(Data).

input([]) --> eos.

:- dynamic numbers/1.

load_data(Numbers) :-
    open('day10/input.dat', read, Stream),
    phrase_from_stream(input(Numbers), Stream).

star(1, N) :-
    load_data(Numbers),
    sort(Numbers, SortedNumbers),
    adapter(SortedNumbers, N1, N3),
    N is N1 * N3.

star(2, N) :-
    load_data(Numbers),
    sort(Numbers, SortedNumbers),
    reverse(SortedNumbers, ReverseNumbers),
    assertz(numbers(ReverseNumbers)),
    max_list(ReverseNumbers, Max),!,
    n_solutions(Max, N).

:- table n_solutions/2.

n_solutions(0, 1) :- !.
n_solutions(N, 0) :-
    numbers(Numbers),
    \+ member(N, Numbers),!.
n_solutions(N, Sols) :-
    X1 is N - 1,
    X2 is N - 2,
    X3 is N - 3,
    n_solutions(X1, Sols1),
    n_solutions(X2, Sols2),
    n_solutions(X3, Sols3),
    Sols is Sols1 + Sols2 + Sols3.


adapter([_Number], 1, 1).
adapter([Number|Numbers], N1, N3) :-
    Numbers = [NumberNext|_],
    Dif is NumberNext - Number,
    adapter(Numbers, N10, N30),
    (Dif = 1 -> N1 is N10+1;N1 is N10),
    (Dif = 3 -> N3 is N30+1;N3 is N30).

next_adapter(Numbers, N, Next) :-
    Next is N-1,
    member(Next, Numbers).

next_adapter(Numbers, N, Next) :-
    Next is N-2,
    member(Next, Numbers).

next_adapter(Numbers, N, Next) :-
    Next is N-3,
    member(Next, Numbers).

:- begin_tests(day10).

test(star1) :- star(1, 1917), !.
test(star2) :- star(2, 113387824750592), !.

:- end_tests(day10).