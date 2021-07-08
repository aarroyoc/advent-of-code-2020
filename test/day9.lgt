:- use_module('../day9/day9.pl').

:- object(day9, extends(lgtunit)).

test(star1, true(X == 400480901)) :- day9:star(1, X).
test(star2, true(X == 67587168)) :- day9:star(2, X).

:- end_object.