:- use_module('../day7/day7.pl').

:- object(day7, extends(lgtunit)).

test(star1, true(X == 119)) :- day7:star(1, X).
test(star2, true(X == 155802)) :- day7:star(2, X).

:- end_object.