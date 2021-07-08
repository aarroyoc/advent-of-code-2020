:- use_module('../day18/day18.pl').

:- object(day18, extends(lgtunit)).

test(star1, true(X == 209335026987)) :- day18:star(1, X).
test(star2, true(X == 33331817392479)) :- day18:star(2, X).

:- end_object.