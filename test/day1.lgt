:- use_module('../day1/day1.pl').

:- object(day1, extends(lgtunit)).

test(star1, true(X == 988771)) :- day1:star(1, X).
test(star2, true(X == 171933104)) :- day1:star(2, X).

:- end_object.