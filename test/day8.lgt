:- use_module('../day8/day8.pl').

:- object(day8, extends(lgtunit)).

test(star1, true(X == 1600)) :- day8:star(1, X).
test(star2, true(X == 1543)) :- day8:star(2, X).

:- end_object.