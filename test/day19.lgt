:- use_module('../day19/day19.pl').

:- object(day19, extends(lgtunit)).

test(star1, true(X == 203)) :- day19:star(1, X).
test(star2, true(X == 304)) :- day19:star(2, X).

:- end_object.