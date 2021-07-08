:- use_module('../day3/day3.pl').

:- object(day3, extends(lgtunit)).

test(star1, true(X == 162)) :- day3:star(1, X).
test(star2, true(X == 3064612320)) :- day3:star(2, X).

:- end_object.