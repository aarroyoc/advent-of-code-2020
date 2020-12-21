:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).



load_data(1, Vertexs) :-
    read_file_to_string('day17/input.dat', String, []),
    string_chars(String, Chars),
    phrase(lines(Lines), Chars),
    lines_vertexs(Lines, Vertexs).

load_data(2, Hypers) :-
    read_file_to_string('day17/input.dat', String, []),
    string_chars(String, Chars),
    phrase(lines(Lines), Chars),
    lines_hypers(Lines, Hypers).

lines_vertexs(Lines, Vertexs) :-
    findall(Vertex, (
        nth0(X, Lines, Line),
        nth0(Y, Line, Value),
        Vertex = vertex(X, Y, 0, Value)
    ),Vertexs).

lines_hypers(Lines, Hypers) :-
    findall(Hyper, (
        nth0(X, Lines, Line),
        nth0(Y, Line, Value),
        Hyper = hyper(X, Y, 0, 0, Value)
    ), Hypers).

star(1, N) :-
    load_data(1, Vertexs),
    loop(6, Vertexs, VertexsOut),
    foldl(count_active, VertexsOut, 0, N).

star(2, N) :-
    load_data(2, Hypers),
    loop2(6, Hypers, HypersOut),
    foldl(count_active2, HypersOut, 0, N).

loop(0, Vertexs, Vertexs).
loop(N, VertexsIn, VertexsOut) :-
    map_expand(VertexsIn, VertexsExpand),
    maplist(step(VertexsExpand), VertexsExpand, VertexsMid),
    NewN is N - 1,
    loop(NewN, VertexsMid, VertexsOut).

loop2(0, Hypers, Hypers).
loop2(N, HypersIn, HypersOut) :-
    map_expand2(HypersIn, HypersExpand),
    maplist(step2(HypersExpand), HypersExpand, HypersMid),
    NewN is N - 1,
    loop2(NewN, HypersMid, HypersOut).

map_expand(Vertexs, VertexsExpand) :-
    bounds(Vertexs, MinX, MaxX, MinY, MaxY, MinZ, MaxZ),
    SafeMinX is MinX - 1,
    SafeMaxX is MaxX + 1,
    SafeMinY is MinY - 1,
    SafeMaxY is MaxY + 1,
    SafeMinZ is MinZ - 1,
    SafeMaxZ is MaxZ + 1,
    findall(NewVertex, (
        between(SafeMinX, SafeMaxX, X),
        between(SafeMinY, SafeMaxY, Y),
        between(SafeMinZ, SafeMaxZ, Z),
        NewVertex = vertex(X, Y, Z, _),
        \+ member(NewVertex, Vertexs),
        NewVertex = vertex(X, Y, Z, '.')
    ),NewVertexs),
    append(Vertexs, NewVertexs, VertexsExpand).

map_expand2(Hypers, HypersExpand) :-
    bounds2(Hypers, MinX, MaxX, MinY, MaxY, MinZ, MaxZ, MinW, MaxW),
    SafeMinX is MinX - 1,
    SafeMaxX is MaxX + 1,
    SafeMinY is MinY - 1,
    SafeMaxY is MaxY + 1,
    SafeMinZ is MinZ - 1,
    SafeMaxZ is MaxZ + 1,
    SafeMinW is MinW - 1,
    SafeMaxW is MaxW + 1,
    findall(NewHyper, (
        between(SafeMinX, SafeMaxX, X),
        between(SafeMinY, SafeMaxY, Y),
        between(SafeMinZ, SafeMaxZ, Z),
        between(SafeMinW, SafeMaxW, W),
        NewHyper = hyper(X, Y, Z, W, _),
        \+ member(NewHyper, Hypers),
        NewHyper = hyper(X, Y, Z, W, '.')
    ),NewHypers),
    append(Hypers, NewHypers, HypersExpand).

bounds([], 0, 0, 0, 0, 0, 0).
bounds([vertex(X, Y, Z, '#')|Vertexs], MinX, MaxX, MinY, MaxY, MinZ, MaxZ) :-
    bounds(Vertexs, MinX0, MaxX0, MinY0, MaxY0, MinZ0, MaxZ0),
    MinX is min(X, MinX0),
    MaxX is max(X, MaxX0),
    MinY is min(Y, MinY0),
    MaxY is max(Y, MaxY0),
    MinZ is min(Z, MinZ0),
    MaxZ is max(Z, MaxZ0).

bounds([vertex(X, Y, Z, '.')|Vertexs], MinX, MaxX, MinY, MaxY, MinZ, MaxZ) :-
    bounds(Vertexs, MinX, MaxX, MinY, MaxY, MinZ, MaxZ).

bounds2([], 0, 0, 0, 0, 0, 0, 0, 0).
bounds2([hyper(X, Y, Z, W, '#')|Hypers], MinX, MaxX, MinY, MaxY, MinZ, MaxZ, MinW, MaxW) :-
    bounds2(Hypers, MinX0, MaxX0, MinY0, MaxY0, MinZ0, MaxZ0, MinW0, MaxW0),
    MinX is min(X, MinX0),
    MaxX is max(X, MaxX0),
    MinY is min(Y, MinY0),
    MaxY is max(Y, MaxY0),
    MinZ is min(Z, MinZ0),
    MaxZ is max(Z, MaxZ0),
    MinW is min(W, MinW0),
    MaxW is max(W, MaxW0).

bounds2([hyper(X, Y, Z, W, '.')|Hypers], MinX, MaxX, MinY, MaxY, MinZ, MaxZ, MinW, MaxW) :-
    bounds2(Hypers, MinX, MaxX, MinY, MaxY, MinZ, MaxZ, MinW, MaxW).

count_active(Vertex, AccIn, AccOut) :-
    (Vertex = vertex(_, _, _, '#') ->
        AccOut is AccIn + 1
    ;   AccOut is AccIn
    ).

count_active2(Hyper, AccIn, AccOut) :-
    (Hyper = hyper(_, _, _, _, '#') ->
        AccOut is AccIn + 1
    ;   AccOut is AccIn
    ).

step(Vertexs, VertexIn, VertexOut) :-
    VertexIn = vertex(X, Y, Z, '#'),
    findall(Active, (
        member(Active, Vertexs),
        Active = vertex(_, _, _, '#'),
        dif(Active, VertexIn),
        neighbour_vertex(Active, VertexIn)
    ), ActiveNeighbours),
    length(ActiveNeighbours, Actives),
    ( (Actives = 2; Actives = 3) ->
        VertexOut = vertex(X, Y, Z, '#')
    ;   VertexOut = vertex(X, Y, Z, '.')
    ).

step(Vertexs, VertexIn, VertexOut) :-
    VertexIn = vertex(X, Y, Z, '.'),
    findall(Active, (
        member(Active, Vertexs),
        Active = vertex(_, _, _, '#'),
        dif(Active, VertexIn),
        neighbour_vertex(Active, VertexIn)
    ), ActiveNeighbours),
    length(ActiveNeighbours, Actives),
    ( (Actives = 3) ->
        VertexOut = vertex(X, Y, Z, '#')
    ;   VertexOut = vertex(X, Y, Z, '.')
    ).

step2(Hypers, HyperIn, HyperOut) :-
    HyperIn = hyper(X, Y, Z, W, '#'),
    findall(Active, (
        member(Active, Hypers),
        Active = hyper(_, _, _, _, '#'),
        dif(Active, HyperIn),
        neighbour_hyper(Active, HyperIn)
    ), ActiveNeighbours),
    length(ActiveNeighbours, Actives),
    ( (Actives = 2; Actives = 3) ->
        HyperOut = hyper(X, Y, Z, W, '#')
    ;   HyperOut = hyper(X, Y, Z, W, '.')
    ).

step2(Hypers, HyperIn, HyperOut) :-
    HyperIn = hyper(X, Y, Z, W, '.'),
    findall(Active, (
        member(Active, Hypers),
        Active = hyper(_, _, _, _, '#'),
        dif(Active, HyperIn),
        neighbour_hyper(Active, HyperIn)
    ), ActiveNeighbours),
    length(ActiveNeighbours, Actives),
    ( (Actives = 3) ->
        HyperOut = hyper(X, Y, Z, W, '#')
    ;   HyperOut = hyper(X, Y, Z, W, '.')
    ).

neighbour_vertex(vertex(X0, Y0, Z0, _), vertex(X1, Y1, Z1, _)) :-
    X is abs(X0 - X1),
    (X = 1;X = 0),
    Y is abs(Y0 - Y1),
    (Y = 1;Y = 0),
    Z is abs(Z0 - Z1),
    (Z = 1;Z = 0).

neighbour_hyper(hyper(X0, Y0, Z0, W0, _), hyper(X1, Y1, Z1, W1, _)) :-
    X is abs(X0 - X1),
    (X = 1;X = 0),
    Y is abs(Y0 - Y1),
    (Y = 1;Y = 0),
    Z is abs(Z0 - Z1),
    (Z = 1;Z = 0),
    W is abs(W0 - W1),
    (W = 1;W = 0).
