:- use_module('../day25/day25.pl').

:- object(day25, extends(lgtunit)).

test(star1, true(X == 3286137)) :- day25:star(1, X).

:- end_object.