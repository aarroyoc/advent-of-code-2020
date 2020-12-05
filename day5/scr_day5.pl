:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(between)).


%% SCRYER 
max_list([Max], Max).
max_list([N|T], Max) :-
    max_list(T, Max1),!,
    Max is max(N, Max1).

rows(128).
columns(8).

input([[[X1, X2, X3, X4, X5, X6, X7], [X8, X9, X10]]|Data]) -->
    [X1],
    [X2],
    [X3],
    [X4],
    [X5],
    [X6],
    [X7],
    [X8],
    [X9],
    [X10],
    "\n",
    input(Data).

input([]) --> call(eos).

eos([], []).

load_data(Passes) :-
    phrase_from_file(input(Passes), 'input.dat').

star(1, X) :-
    load_data(Passes),
    maplist(seat, Passes, Seats),
    maplist(seat_id, Seats, SeatIDs),
    max_list(SeatIDs, X).

star(2, N) :-
    load_data(Passes),
    maplist(seat, Passes, Seats),
    findall(MissingSeat,(
        between(20, 107, N),
        between(0, 7, M),
        MissingSeat = seat(N, M),
        \+ member(MissingSeat, Seats)
    ), MissingSeats),
    [Seat|_] = MissingSeats,
    seat_id(Seat, N).

format_seat([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10], [[X1, X2, X3, X4, X5, X6, X7], [X8, X9, X10]]).

seat([PassRow, PassColumn], seat(Row, Column)) :-
    rows(R),
    columns(C),
    seat_calc(PassRow, PassColumn, Row, Column, R, C).

seat_calc([], [], 0, 0, _, _).

seat_calc([], ['L'|PassColumn], 0, Column, _, M) :-
    Middle is M / 2,
    seat_calc([], PassColumn, 0, Column, _, Middle).

seat_calc([], ['R'|PassColumn], 0, Column, _, M) :-
    Middle is M / 2,
    seat_calc([], PassColumn, 0, Column1, _, Middle),
    Column is Middle + Column1.

seat_calc(['F'|PassRow], PassColumn, Row, Column, N, M) :-
    Middle is N / 2,
    seat_calc(PassRow, PassColumn, Row, Column, Middle, M).

seat_calc(['B'|PassRow], PassColumn, Row, Column, N, M) :-
    Middle is N / 2,
    seat_calc(PassRow, PassColumn, Row1, Column, Middle, M),
    Row is Row1 + Middle.

seat_id(seat(Row, Column), SeatID) :-
    SeatID is (Row * 8) + Column.
