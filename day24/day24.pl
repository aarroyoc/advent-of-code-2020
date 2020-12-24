:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input_tile([]) --> "\n".

input_tile([e|Steps]) -->
    "e",
    input_tile(Steps).

input_tile([se|Steps]) -->
    "se",
    input_tile(Steps).

input_tile([sw|Steps]) -->
    "sw",
    input_tile(Steps).

input_tile([w|Steps]) -->
    "w",
    input_tile(Steps).

input_tile([nw|Steps]) -->
    "nw",
    input_tile(Steps).

input_tile([ne|Steps]) -->
    "ne",
    input_tile(Steps).

input([Tile|Tiles]) -->
    input_tile(Tile),
    input(Tiles).

input([]) --> [].

load_data(Tiles) :-
    read_file_to_string('day24/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Tiles), Chars).

star(1, N) :-
    load_data(StepsTiles),
    maplist(steps_tile, StepsTiles, Tiles),
    flip_tiles(Tiles, BlackTiles),
    length(BlackTiles, N).

star(2, N) :-
    load_data(StepsTiles),
    maplist(steps_tile, StepsTiles, Tiles),
    flip_tiles(Tiles, BlackTiles),
    steps(100, BlackTiles, MapOut),
    length(MapOut, N).

steps(0, Map, Map).
steps(N, Map, MapOut) :-
    length(Map, M),
    format('~d: ~d', [N, M]),nl,
    include(keep_black(Map), Map, MapOut0),
    maplist(white_tiles(Map), Map, WhiteTiles0),
    flatten(WhiteTiles0, WhiteTiles1),
    list_to_set(WhiteTiles1, WhiteTiles),
    include(flip_black(Map), WhiteTiles, MapOut1),
    append(MapOut0, MapOut1, MapOut2),
    NewN is N - 1,!,
    steps(NewN, MapOut2, MapOut).

direction([e]).
direction([w]).
direction([se]).
direction([ne]).
direction([sw]).
direction([nw]).

white_tiles(Map, X-Y, WhiteTails) :-
    findall(Tile, (
        direction(Direction),
        step_tiles_(X-Y, Direction, Tile),
        \+ member(Tile, Map)
    ), WhiteTails).

flip_black(Map, X-Y) :-
    findall(Tile, (
        direction(Direction),
        step_tiles_(X-Y, Direction, Tile),
        member(Tile, Map)
    ), Tiles),
    length(Tiles, 2).

keep_black(Map, X-Y) :-
    findall(Tile, (
        direction(Direction),
        step_tiles_(X-Y, Direction, Tile),
        member(Tile, Map)
    ), Tiles),
    length(Tiles, N),
    (N = 1; N = 2).

steps_tile(Steps, Tile) :-
    step_tiles_(0-0, Steps, Tile).

step_tiles_(X-Y, [], X-Y).
step_tiles_(X-Y, [w|Steps], Tile) :-
    NewX is X - 1,
    step_tiles_(NewX-Y, Steps, Tile).
step_tiles_(X-Y, [e|Steps], Tile) :-
    NewX is X + 1,
    step_tiles_(NewX-Y, Steps, Tile).
step_tiles_(X-Y, [sw|Steps], Tile) :-
    NewX is X - (Y mod 2),
    NewY is Y - 1,
    step_tiles_(NewX-NewY, Steps, Tile).
step_tiles_(X-Y, [se|Steps], Tile) :-
    NewX is X + (1 - (Y mod 2)),
    NewY is Y - 1,
    step_tiles_(NewX-NewY, Steps, Tile).
step_tiles_(X-Y, [nw|Steps], Tile) :-
    NewX is X - (Y mod 2),
    NewY is Y + 1,
    step_tiles_(NewX-NewY, Steps, Tile).
step_tiles_(X-Y, [ne|Steps], Tile) :-
    NewX is X + (1 - (Y mod 2)),
    NewY is Y + 1,
    step_tiles_(NewX-NewY, Steps, Tile).

flip_tiles([], []).
flip_tiles([Tile|Tiles], BlackTiles) :-
    flip_tiles(Tiles, BlackTiles0),
    (member(Tile, BlackTiles0) ->
        delete(BlackTiles0, Tile, BlackTiles)
    ;   BlackTiles = [Tile|BlackTiles0]
    ).
