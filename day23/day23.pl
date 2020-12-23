star(1, N) :-
    Start = [3, 1, 8, 9, 4, 6, 5, 7, 2],
    moves(3, 100, Start, End),
    order(End, Order),
    append(N, [1], Order).

star(2, N) :-
    list(10, 1000000, Post),
    append([3, 8, 9, 1, 2, 5, 4, 6, 7], Post, Start),
    moves2(3, 10000000, Start, End),
    next(1, X, End),
    next(X, Y, End),
    N is X * Y.

moves(_, 0, End, End).
moves(CurrentCup, N, Start, End) :-
    next(CurrentCup, Cup1, Start),
    next(Cup1, Cup2, Start),
    next(Cup2, Cup3, Start),
    selectchk(Cup1, Start, Start0),
    selectchk(Cup2, Start0, Start1),
    selectchk(Cup3, Start1, Start2),
    Picked = [Cup1, Cup2, Cup3],
    destination_cup(CurrentCup, Start2, DestinationCup),
    append(Pre, Post, Start2),
    last(Pre, DestinationCup),
    append(Pre, Picked, PreEnd),
    append(PreEnd, Post, End0),
    NewN is N - 1,
    next(CurrentCup, NewCurrentCup, End0),!,
    moves(NewCurrentCup, NewN, End0, End).


moves2(_, 0, End, End).
moves2(CurrentCup, N, Start, End) :-
    next(CurrentCup, Cup1, Start),
    next(Cup1, Cup2, Start),
    next(Cup2, Cup3, Start),
    selectchk(Cup1, Start, Start0),
    selectchk(Cup2, Start0, Start1),
    selectchk(Cup3, Start1, Start2),
    Picked = [Cup1, Cup2, Cup3],
    destination_cup2(CurrentCup, Picked, DestinationCup),
    append(Pre, Post, Start2),
    last(Pre, DestinationCup),
    append(Pre, Picked, PreEnd),
    append(PreEnd, Post, End0),
    NewN is N - 1,
    next(CurrentCup, NewCurrentCup, End0),!,
    (0 is N mod 100000 ->
        (write(N),nl)
    ;   true
    ),
    moves2(NewCurrentCup, NewN, End0, End).

next(Prev, Succ, List) :-
    nextto(Prev, Succ, List).

next(Prev, Succ, List) :-
    last(List, Prev),
    List = [Succ|_].


destination_cup(0, Start, N) :-
    max_list(Start, N).
destination_cup(CurrentCup, Start, Out) :-
    N is CurrentCup - 1,
    (member(N, Start) ->
        Out is N
    ;   destination_cup(N, Start, Out)
    ).

destination_cup2(0, Picked, Out) :-
    (Out = 1000000;Out = 999999;Out = 999998),
    \+ member(Out, Picked).
destination_cup2(CurrentCup, Picked, Out) :-
    N is CurrentCup - 1,
    (memberchk(N, Picked) ->
        destination_cup2(N, Picked, Out)
    ;   Out is N
    ).

order(End, [N|Order]) :-
    next(1, N, End),
    order_(N, End, Order).

order_(1, _, []).
order_(N, End, [M|Order]) :-
    next(N, M, End),
    order_(M, End, Order).

list(End, End, [End]).
list(N, End, [N|List]) :-
    NewN is N + 1,
    list(NewN, End, List).
