:- module(day2, []).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([password(Password, Letter, Min, Max)|Data]) -->
    integer(Min),
    "-",
    integer(Max),
    " ",
    string(Letter),
    ":",
    !,
    " ",
    string(Password),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Data) :-
    open('day2/input.dat', read, Stream),
    phrase_from_stream(input(Data), Stream).

star(1, X) :-
    load_data(Passwords),
    findall(Password, (
        member(Password, Passwords),
        policy(Password)
    ),ValidPasswords),
    length(ValidPasswords, X).

star(2, X) :-
    load_data(Passwords),
    findall(Password, (
        member(Password, Passwords),
        policy2(Password)
    ),ValidPasswords),
    length(ValidPasswords, X).

count(_, [], 0) :- !.
count(A, [A|Chars], N) :-
    !,count(A, Chars, N1),
    N is N1 + 1.
count(A, [_Char|Chars], N) :-
    count(A, Chars, N).

policy(password(Password, Letter, Min, Max)) :-
    string_chars(Password, PasswordChars),
    string_chars(Letter, [SingleLetter]),
    count(SingleLetter, PasswordChars, N),
    N >= Min,
    N =< Max.

policy2(password(Password, Letter, A, B)) :-
    string_chars(Password, Text),
    string_chars(Letter, [SingleLetter]),
    nth1(A, Text, SingleLetter),
    \+ nth1(B, Text, SingleLetter).

policy2(password(Password, Letter, A, B)) :-
    string_chars(Password, Text),
    string_chars(Letter, [SingleLetter]),
    \+ nth1(A, Text, SingleLetter),
    nth1(B, Text, SingleLetter).

:- begin_tests(day2).

test(star1) :- star(1, 569), !.
test(star2) :- star(2, 346), !.

:- end_tests(day2).