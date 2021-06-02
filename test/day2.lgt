:- use_module('../day2/day2.pl').

:- object(day2, extends(lgtunit)).

test(star1) :- day2:star(1, 569).
test(star2) :- day2:star(2, 346).

:- end_object.