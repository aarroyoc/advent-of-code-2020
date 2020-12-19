:- module(day19, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input_rules([rule(ID, char(C))|Rules]) -->
    integer(ID),
    ": \"",
    [C],
    "\"\n",
    input_rules(Rules).

input_rules([rule(ID, or(Subrule1, Subrule2))|Rules]) -->
    integer(ID),
    ":",
    input_subrule(Subrule1),
    " |",
    input_subrule(Subrule2),
    "\n",
    input_rules(Rules).

input_rules([rule(ID, single(Subrule))|Rules]) -->
    integer(ID),
    ":",
    input_subrule(Subrule),
    "\n",
    input_rules(Rules).

input_rules([]) --> [].

input_subrule([N|Subrule]) -->
    " ",
    integer(N),
    input_subrule(Subrule).

input_subrule([]) --> [].

input_messages([Msg|Messages]) -->
    string_without("\n", Msg),
    "\n",
    input_messages(Messages).

input_messages([]) --> [].

input(Rules, Messages) -->
    input_rules(Rules),
    "\n",
    input_messages(Messages).

load_data(1, Rules, Messages) :-
    read_file_to_string('day19/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Rules, Messages), Chars).

load_data(2, Rules, Messages) :-
    read_file_to_string('day19/input2.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Rules, Messages), Chars).

star(X, N) :-
    load_data(X, Rules, Messages),!,
    include(phrase(msg(0, Rules)), Messages, ValidMessages),
    length(ValidMessages, N).

msg(ID, Rules) -->
    {
        member(rule(ID, char(C)), Rules)
    },
    [C].

msg(ID, Rules) -->
    {
        member(rule(ID, or(Rule1, Rule2)), Rules),
        Rule1 = [Rule11, Rule12],
        Rule2 = [Rule21, Rule22]
    },(
    ( msg(Rule11, Rules), msg(Rule12, Rules)) | 
    ( msg(Rule21, Rules), msg(Rule22, Rules))).

msg(ID, Rules) -->
    {
        member(rule(ID, or(Rule1, Rule2)), Rules),
        Rule1 = [Rule11, Rule12],
        Rule2 = [Rule21, Rule22, Rule23]
    },(
    ( msg(Rule11, Rules), msg(Rule12, Rules)) | 
    ( msg(Rule21, Rules), msg(Rule22, Rules), msg(Rule23, Rules))).

msg(ID, Rules) -->
    {
        member(rule(ID, or(Rule1, Rule2)), Rules),
        Rule1 = [Rule11],
        Rule2 = [Rule21]
    },(
    ( msg(Rule11, Rules) ) |
    ( msg(Rule21, Rules) ) ).

msg(ID, Rules) -->
    {
        member(rule(ID, or(Rule1, Rule2)), Rules),
        Rule1 = [Rule11],
        Rule2 = [Rule21, Rule22]
    },(
    ( msg(Rule11, Rules) ) |
    ( msg(Rule21, Rules), msg(Rule22, Rules) ) ).

msg(ID, Rules) -->
    {
        member(rule(ID, single(Rule)), Rules),
        Rule = [Rule1, Rule2, Rule3]
    },
    msg(Rule1, Rules),
    msg(Rule2, Rules),
    msg(Rule3, Rules).

msg(ID, Rules) -->
    {
        member(rule(ID, single(Rule)), Rules),
        Rule = [Rule1, Rule2]
    },
    msg(Rule1, Rules),
    msg(Rule2, Rules).

msg(ID, Rules) -->
    {
        member(rule(ID, single(Rule)), Rules),
        Rule = [Rule1]
    },
    msg(Rule1, Rules).

:- begin_tests(day19).

test(star1) :- star(1, 203),!.
test(star2) :- star(2, 304),!.

:- end_tests(day19).
