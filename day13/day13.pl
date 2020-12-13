:- module(day13, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

input(ArriveTime, Buses) -->
    integer(ArriveTime),
    "\n",
    input_bus(Buses),
    "\n".

input_bus([Bus|Buses]) -->
    (integer(Bus) | ("x", { Bus = x }) ),
    ",",
    input_bus(Buses).

input_bus([Bus]) -->
    (integer(Bus) | ("x", { Bus = x })).

load_data(ArriveTime, Buses) :-
    read_file_to_string('day13/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(ArriveTime, Buses), Chars).

star(1, N) :-
    load_data(ArriveTime, Buses),
    maplist(multiply_until(ArriveTime), Buses, NearTime),
    min_list(NearTime, MinTime),
    nth0(BusN, NearTime, MinTime),
    nth0(BusN, Buses, BusID),
    N is BusID * (MinTime - ArriveTime).

star(2, N) :-
    load_data(_, Buses),
    build_pairs(0, Buses, Pairs),
    crt(Pairs, N).

multiply_until(_, x, 999999999999) :-!.
multiply_until(ArriveTime, Bus, NearTime) :-
    multiply_until_(ArriveTime, Bus, Bus, NearTime).

multiply_until_(ArriveTime, Bus, Time, NearTime) :-
    NewTime is Time + Bus,
    (ArriveTime =< NewTime -> 
            NearTime = NewTime ;
            multiply_until_(ArriveTime, Bus, NewTime, NearTime)).

build_pairs(_, [], []).
build_pairs(N, [x|Buses], Pairs) :-
    NewN is N - 1,
    build_pairs(NewN, Buses, Pairs).
build_pairs(N, [Bus|Buses], [Pair|Pairs]) :-
    Pair = N-Bus,
    NewN is N - 1,
    build_pairs(NewN, Buses, Pairs).

% taken from https://rosettacode.org/wiki/Chinese_remainder_theorem#Prolog

product(A, B, C) :- C is A*B.
 
pair(X, Y, X-Y).
 
egcd(_, 0, 1, 0) :- !.
egcd(A, B, X, Y) :-
    divmod(A, B, Q, R),
    egcd(B, R, S, X),
    Y is S - Q*X.
 
modinv(A, B, X) :-
    egcd(A, B, X, Y),
    A*X + B*Y =:= 1.
 
crt_fold(A, M, P, R0, R1) :- % system of equations of (x = a) (mod m); p = M/m
    modinv(P, M, Inv),
    R1 is R0 + A*Inv*P.
 
crt(Pairs, N) :-
    maplist(pair, As, Ms, Pairs),
    foldl(product, Ms, 1, M),
    maplist(divmod(M), Ms, Ps, _), % p(n) <- M/m(n)
    foldl(crt_fold, As, Ms, Ps, 0, N0),
    N is N0 mod M.


:- begin_tests(day13).

test(star1) :- star(1, 119), !.
test(star2) :- star(2, 1106724616194525), !.

:- end_tests(day13).