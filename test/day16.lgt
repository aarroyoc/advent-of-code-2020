:- use_module('../day16/day16.pl').

:- object(day16, extends(lgtunit)).

test(star1, true(X == 20231)) :- day16:star(1, X).
test(star2, true(X == 1940065747861)) :- day16:star(2, X).

:- end_object.