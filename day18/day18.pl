:- module(day18, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([X|Cs]) --> integer(X),!,line(Cs).
line(Cs) --> [C], line_(C, Cs).
line_(*, [*|Cs])     --> line(Cs).
line_(+, [+|Cs])     --> line(Cs).
line_('(', ['('|Cs]) --> line(Cs).
line_(')', [')'|Cs]) --> line(Cs).
line_(' ', Cs)       --> line(Cs).

eos([], []).


load_data(Lines) :-
    read_file_to_string('day18/input.dat', String, []),
    string_chars(String, Chars),
    phrase(lines(Lines), Chars).

star(1, N) :-
    load_data(Lines),
    maplist(execute_line, Lines, Results),
    sum_list(Results, N).

star(2, N) :-
    load_data(Lines),
    maplist(execute_line2, Lines, Results),
    sum_list(Results, N).

execute_line(Line, Result) :-
    phrase(eval(0, Result), Line).

execute_line2(Line, Result) :-
    phrase(expr(Result), Line).

eval(0, Out) -->
    [N],
    {
        integer(N)
    },
    eval(N, Out).

eval(0, Out) -->
    "(",
    eval(0, N),
    ")",
    eval(N, Out).

eval(In, Out) -->
    "+",
    [N],
    {
        integer(N),
        X is N + In
    },
    eval(X, Out).

eval(In, Out) -->
    "*",
    [N],
    {
        integer(N),
        X is N * In
    },
    eval(X, Out).

eval(In, Out) -->
    "+",
    "(",
    eval(0, N),
    ")",
    {
        X is N + In
    },
    eval(X, Out).

eval(In, Out) -->
    "*",
    "(",
    eval(0, N),
    ")",
    {
        X is N * In
    },
    eval(X, Out).

eval(X, X) --> [].

:- table expr//1.
:- table sum_expr//1.

expr(X) -->
    "(",
    expr(X),
    ")".

expr(X) -->
    sum_expr(X1),
    "*",
    expr(X2),
    {
        X is X1 * X2
    }.

expr(X) --> sum_expr(X).

expr(X) --> [X], {
    number(X)
}.

sum_expr(X) -->
    sum_expr(X1),
    "+",
    sum_expr(X2),{
        X is X1 + X2
    }.

sum_expr(X) --> [X], {
    number(X)
}.
sum_expr(X) -->
    "(",
    expr(X),
    ")".

:- begin_tests(day18).

test(star1) :- star(1, 209335026987),!.
test(star2) :- star(2, 33331817392479),!.

:- end_tests(day18).