:- use_module('../day4/day4.pl').

:- object(day4, extends(lgtunit)).

test(star1, true(X == 245)) :- day4:star(1, X).
test(star2, true(X == 133)) :- day4:star(2, X).

:- end_object.