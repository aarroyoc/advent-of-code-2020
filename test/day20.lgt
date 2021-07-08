:- use_module('../day20/day20.pl').

:- object(day20, extends(lgtunit)).

test(star1, true(X == 108603771107737)) :- day20:star(1, X).
test(star2, true(X == 2129)) :- day20:star(2, X).

:- end_object.