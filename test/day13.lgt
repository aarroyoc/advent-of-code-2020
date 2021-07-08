:- use_module('../day13/day13.pl').

:- object(day13, extends(lgtunit)).

test(star1, true(X == 119)) :- day13:star(1, X).
test(star2, true(X == 1106724616194525)) :- day13:star(2, X).

:- end_object.