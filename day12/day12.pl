:- module(day12, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

input([]) --> eos.

input([step('N', Arg)|Steps]) -->
    "N",
    integer(Arg),
    "\n",
    input(Steps).

input([step('S', Arg)|Steps]) -->
    "S",
    integer(Arg),
    "\n",
    input(Steps).

input([step('E', Arg)|Steps]) -->
    "E",
    integer(Arg),
    "\n",
    input(Steps).

input([step('W', Arg)|Steps]) -->
    "W",
    integer(Arg),
    "\n",
    input(Steps).

input([step('L', Arg)|Steps]) -->
    "L",
    integer(Arg),
    "\n",
    input(Steps).

input([step('R', Arg)|Steps]) -->
    "R",
    integer(Arg),
    "\n",
    input(Steps).

input([step('F', Arg)|Steps]) -->
    "F",
    integer(Arg),
    "\n",
    input(Steps).

load_data(Steps) :-
    read_file_to_string('day12/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Steps), Chars).

star(1, N) :-
    load_data(Steps),!,
    StateIn = state('E', 0, 0),
    execute(Steps, StateIn, StateOut),
    StateOut = state(_, X, Y),
    N is abs(X)+abs(Y).

star(2, N) :-
    load_data(Steps),!,
    StateIn = state('E', ship(0, 0), waypoint(-10, 1)),
    execute2(Steps, StateIn, StateOut),
    StateOut = state(_, ship(X, Y), waypoint(_, _)),
    N is abs(X)+abs(Y).

execute([], StateOut, StateOut).

execute([step('N', Arg)|Steps], state(Face, X, Y), StateOut) :-
    NewY is Y + Arg,
    NewState = state(Face, X, NewY),
    execute(Steps, NewState, StateOut).

execute([step('S', Arg)|Steps], state(Face, X, Y), StateOut) :-
    NewY is Y - Arg,
    NewState = state(Face, X, NewY),
    execute(Steps, NewState, StateOut).

execute([step('E', Arg)|Steps], state(Face, X, Y), StateOut) :-
    NewX is X - Arg,
    NewState = state(Face, NewX, Y),
    execute(Steps, NewState, StateOut).

execute([step('W', Arg)|Steps], state(Face, X, Y), StateOut) :-
    NewX is X + Arg,
    NewState = state(Face, NewX, Y),
    execute(Steps, NewState, StateOut).

execute([step('L', Arg)|Steps], state(Face, X, Y), StateOut) :-
    face('L', Arg, Face, NewFace),
    NewState = state(NewFace, X, Y),
    execute(Steps, NewState, StateOut).

execute([step('R', Arg)|Steps], state(Face, X, Y), StateOut) :-
    face('R', Arg, Face, NewFace),
    NewState = state(NewFace, X, Y),
    execute(Steps, NewState, StateOut).

execute([step('F', Arg)|Steps], state(Face, X, Y), StateOut) :-
    execute([step(Face, Arg)|Steps], state(Face, X, Y), StateOut).

execute2([], StateOut, StateOut).

execute2([step('N', Arg)|Steps], state(Face, Ship, waypoint(X, Y)), StateOut) :-
    NewY is Y + Arg,
    NewState = state(Face, Ship, waypoint(X, NewY)),
    execute2(Steps, NewState, StateOut).

execute2([step('S', Arg)|Steps], state(Face, Ship, waypoint(X, Y)), StateOut) :-
    NewY is Y - Arg,
    NewState = state(Face, Ship, waypoint(X, NewY)),
    execute2(Steps, NewState, StateOut).

execute2([step('E', Arg)|Steps], state(Face, Ship, waypoint(X, Y)), StateOut) :-
    NewX is X - Arg,
    NewState = state(Face, Ship, waypoint(NewX, Y)),
    execute2(Steps, NewState, StateOut).

execute2([step('W', Arg)|Steps], state(Face, Ship, waypoint(X, Y)), StateOut) :-
    NewX is X + Arg,
    NewState = state(Face, Ship, waypoint(NewX, Y)),
    execute2(Steps, NewState, StateOut).

execute2([step('L', Arg)|Steps], state(Face, Ship, waypoint(X, Y)), StateOut) :-
    face2('L', Arg, X, Y, NewWX, NewWY),
    execute2(Steps, state(Face, Ship, waypoint(NewWX, NewWY)), StateOut).

execute2([step('R', Arg)|Steps], state(Face, Ship, waypoint(X, Y)), StateOut) :-
    face2('R', Arg, X, Y, NewWX, NewWY),
    execute2(Steps, state(Face, Ship, waypoint(NewWX, NewWY)), StateOut).

execute2([step('F', Arg)|Steps], state(Face, ship(X, Y), waypoint(WX, WY)), StateOut) :-
    NewX is X + WX*Arg,
    NewY is Y + WY*Arg,
    execute2(Steps, state(Face, ship(NewX, NewY), waypoint(WX, WY)), StateOut).

face('R', 90, 'E', 'S').
face('R', 90, 'S', 'W').
face('R', 90, 'W', 'N').
face('R', 90, 'N', 'E').

face('L', 90, 'E', 'N').
face('L', 90, 'N', 'W').
face('L', 90, 'W', 'S').
face('L', 90, 'S', 'E').

face(Rotation, 180, Face, NewFace) :-
    face(Rotation, 90, Face, Face1),
    face(Rotation, 90, Face1, NewFace).

face(Rotation, 270, Face, NewFace) :-
    face(Rotation, 90, Face, Face1),
    face(Rotation, 90, Face1, Face2),
    face(Rotation, 90, Face2, NewFace).

face2('R', 90, X, Y, NewX, NewY) :-
    NewY is X,
    NewX is Y * -1.

face2('L', 90, X, Y, NewX, NewY) :-
    NewX is Y,
    NewY is X * -1.

face2(Rotation, 180, X, Y, NewX, NewY) :-
    face2(Rotation, 90, X, Y, X1, Y1),
    face2(Rotation, 90, X1, Y1, NewX, NewY).

face2(Rotation, 270, X, Y, NewX, NewY) :-
    face2(Rotation, 90, X, Y, X1, Y1),
    face2(Rotation, 90, X1, Y1, X2, Y2),
    face2(Rotation, 90, X2, Y2, NewX, NewY).

:- begin_tests(day12).

test(star1) :- star(1, 1007),!.
test(star2) :- star(2, 41212),!.

:- end_tests(day12).