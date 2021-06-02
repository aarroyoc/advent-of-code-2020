:- use_module('../day1/day1.pl').

:- object(day1, extends(lgtunit)).

test(star1) :- day1:star(1, 988771).
test(star2) :- day1:star(2, 171933104).

:- end_object.