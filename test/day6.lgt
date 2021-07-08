:- use_module('../day6/day6.pl').

:- object(day6, extends(lgtunit)).

test(star1, true(X == 6612)) :- day6:star(1, X).
test(star2, true(X == 3268)) :- day6:star(2, X).

:- end_object.