:- use_module('../day5/day5.pl').

:- object(day5, extends(lgtunit)).

test(star1, true(X == 974)) :- day5:star(1, X).
test(star2, true(X == 646)) :- day5:star(2, X).

:- end_object.