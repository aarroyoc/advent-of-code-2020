:- module(day8, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).
:- use_module(library(ordsets)).

input([]) --> eos.

input([step(acc, Arg)|Steps]) -->
    "acc",
    " +",
    integer(Arg),
    "\n",
    input(Steps).

input([step(jmp, Arg)|Steps]) -->
    "jmp",
    " +",
    integer(Arg),
    "\n",
    input(Steps).

input([step(nop, Arg)|Steps]) -->
    "nop",
    " +",
    integer(Arg),
    "\n",
    input(Steps).

input([step(acc, Arg)|Steps]) -->
    "acc",
    " -",
    integer(Arg0),
    {
        Arg is Arg0 * -1
    },
    "\n",
    input(Steps).

input([step(jmp, Arg)|Steps]) -->
    "jmp",
    " -",
    integer(Arg0),
    {
        Arg is Arg0 * -1
    },
    "\n",
    input(Steps).

input([step(nop, Arg)|Steps]) -->
    "nop",
    " -",
    integer(Arg0),
    {
        Arg is Arg0 * -1
    },
    "\n",
    input(Steps).

load_data(Steps) :-
    read_file_to_string('day8/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Steps), Chars).

star(1, X) :-
    load_data(Steps),!,
    Pc is 0,
    AccIn is 0,
    Visited = [],
    execute(Pc, Steps, AccIn, Visited, AccOut),
    X is AccOut.

star(2, X) :-
    load_data(Steps),!,
    Pc is 0,
    AccIn is 0,
    Visited = [],
    length(Steps, L1),
    L is L1 - 1,
    !,
    between(0, L, Exchange),
    exchanged_steps(Steps, ExchangedSteps, Exchange, 0),
    execute2(Pc, ExchangedSteps, AccIn, Visited, AccOut),
    X is AccOut.

execute(Pc, _Steps, AccIn, Visited, AccOut) :-
    length(Visited, L1),
    ord_add_element(Visited, Pc, NewVisited),
    length(NewVisited, L1),
    AccOut = AccIn.

execute(Pc, Steps, AccIn, Visited, AccOut) :-
    nth0(Pc, Steps, step(nop, _)),
    ord_add_element(Visited, Pc, NewVisited),
    NewPc is Pc + 1,
    execute(NewPc, Steps, AccIn, NewVisited, AccOut).

execute(Pc, Steps, AccIn, Visited, AccOut) :-
    nth0(Pc, Steps, step(acc, Int)),
    ord_add_element(Visited, Pc, NewVisited),
    NewPc is Pc + 1,
    NewAccIn is AccIn + Int,
    execute(NewPc, Steps, NewAccIn, NewVisited, AccOut).

execute(Pc, Steps, AccIn, Visited, AccOut) :-
    nth0(Pc, Steps, step(jmp, Int)),
    ord_add_element(Visited, Pc, NewVisited),
    NewPc is Pc + Int,
    execute(NewPc, Steps, AccIn, NewVisited, AccOut).

execute2(Pc, _Steps, _AccIn, Visited, _AccOut) :-
    length(Visited, L1),
    ord_add_element(Visited, Pc, NewVisited),
    length(NewVisited, L1),
    !,
    fail.

execute2(Pc, Steps, AccIn, _Visited, AccOut) :-
    \+ nth0(Pc, Steps, _),
    AccOut = AccIn.

execute2(Pc, Steps, AccIn, Visited, AccOut) :-
    nth0(Pc, Steps, step(nop, _)),
    ord_add_element(Visited, Pc, NewVisited),
    NewPc is Pc + 1,
    execute2(NewPc, Steps, AccIn, NewVisited, AccOut).

execute2(Pc, Steps, AccIn, Visited, AccOut) :-
    nth0(Pc, Steps, step(acc, Int)),
    ord_add_element(Visited, Pc, NewVisited),
    NewPc is Pc + 1,
    NewAccIn is AccIn + Int,
    execute2(NewPc, Steps, NewAccIn, NewVisited, AccOut).

execute2(Pc, Steps, AccIn, Visited, AccOut) :-
    nth0(Pc, Steps, step(jmp, Int)),
    ord_add_element(Visited, Pc, NewVisited),
    NewPc is Pc + Int,
    execute2(NewPc, Steps, AccIn, NewVisited, AccOut).

exchange_step(step(nop, Arg), step(jmp, Arg)) :- Arg \= 0.
exchange_step(step(jmp, Arg), step(nop, Arg)).

exchanged_steps([], [], _, _).
exchanged_steps([Step|Steps], [NewStep|Steps], Exchange, Exchange) :-
    exchange_step(Step, NewStep).

exchanged_steps([Step|StepIn], [Step|StepOut], Exchange, N) :-
    Exchange \= N,
    N0 is N + 1,
    exchanged_steps(StepIn, StepOut, Exchange, N0).

:- begin_tests(day8).

test(star1) :- star(1, 1600),!.
test(star2) :- star(2, 1543),!.

:- end_tests(day8).