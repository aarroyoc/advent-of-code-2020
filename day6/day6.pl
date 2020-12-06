:- module(day6, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([[]]) --> eos.
input([[] | Data]) --> `\n`, input(Data). 

input(NewData) -->
    string(Person),
    `\n`,
    input(Data),
    {
        Data = [Group|OldData],
        string_chars(Person, Chars),
        append(Group, [Chars], NewGroup),
        NewData = [NewGroup|OldData]
    }.

load_data(Answers) :-
    open('day6/input.dat', read, Stream),
    phrase_from_stream(input(Answers), Stream).

star(1, X) :-
    load_data(Answers),!,
    maplist(unique_answers, Answers, UniqueAnswers),
    maplist(length, UniqueAnswers, NumUniqueAnswers),
    sum_list(NumUniqueAnswers, X).

star(2, X) :-
    load_data(Answers),!,
    maplist(everyone_answers, Answers, EveryoneAnswers),
    maplist(length, EveryoneAnswers, NumUniqueAnswers),
    sum_list(NumUniqueAnswers, X).

unique_answers(Answers, UniqueAnswers) :-
    foldl(append, Answers, [], AnswerList),
    list_to_set(AnswerList, UniqueAnswers).

everyone_answers(Answers, EveryoneAnswers) :-
    unique_answers(Answers, UniqueAnswers),
    convlist(filter_answer(Answers), UniqueAnswers, EveryoneAnswers).

filter_answer(Answers, Unique, Unique) :-
    forall(member(Answer, Answers), member(Unique, Answer)).

:- begin_tests(day6).

test(star1) :- star(1, 6612),!.
test(star2) :- star(2, 3268),!.

:- end_tests(day6).