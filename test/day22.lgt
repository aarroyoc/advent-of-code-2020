:- use_module('../day22/day22.pl').

:- object(day22, extends(lgtunit)).

test(star1, true(X == 33400)) :- day22:star(1, X).
test(star2, true(X == 33745)) :- day22:star(2, X).

:- end_object.