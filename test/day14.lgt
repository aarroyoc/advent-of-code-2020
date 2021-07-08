:- use_module('../day14/day14.pl').

:- object(day14, extends(lgtunit)).

test(star1, true(X == 11926135976176)) :- day14:star(1, X).
test(star2, true(X == 4330547254348)) :- day14:star(2, X).

:- end_object.