:- use_module('../day12/day12.pl').

:- object(day12, extends(lgtunit)).

test(star1, true(X == 1007)) :- day12:star(1, X).
test(star2, true(X == 41212)) :- day12:star(2, X).

:- end_object.