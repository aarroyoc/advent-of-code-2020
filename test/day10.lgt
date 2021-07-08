:- use_module('../day10/day10.pl').

:- object(day10, extends(lgtunit)).

test(star1, true(X == 1917)) :- day10:star(1, X).
test(star2, true(X == 113387824750592)) :- day10:star(2, X).

:- end_object.