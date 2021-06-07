:- use_module('../day2/day2.pl').

:- object(day2, extends(lgtunit)).

test(star1, true(X == 569)) :- day2:star(1, X).
test(star2, true(X == 346)) :- day2:star(2, X).

:- end_object.