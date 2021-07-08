:- use_module('../day15/day15.pl').

:- object(day15, extends(lgtunit)).

test(star1, true(X == 700)) :- day15:star(1, X).
test(star2, true(X == 51358)) :- day15:star(2, X).

:- end_object.