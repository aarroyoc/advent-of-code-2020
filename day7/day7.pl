:- module(day7, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(tabling)).
:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

input([]) --> eos.

input([[BagHead, BagRule]|Data]) -->
    string(BagHead),
    " bags contain ",
    input_bag(BagRule),
    input(Data).

input_bag([]) -->
    ".\n".

input_bag([]) -->
    "no other bags.\n".

input_bag([BagRule|BagRules]) -->
    integer(BagNumber),
    " ",
    string(BagBody),
    (" bag, "|" bags, "|" bag" | " bags"),
    {
        BagRule = [BagNumber, BagBody]
    },
    input_bag(BagRules).

load_data(Rules) :-
    read_file_to_string('day7/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Rules), Chars).

star(1, X) :-
    load_data(Rules),!,
    convlist(find_shiny_gold(Rules), Rules, GoldRules),
    length(GoldRules, X).

star(2, X) :-
    load_data(Rules),!,
    bag_count(Rules, [1, "shiny gold"], X0),
    X is X0-1.

:- table find_shiny_gold/3.

find_shiny_gold(_Rules, Rule, Rule) :-
    Rule = [_BagHead, BagRule],
    member([_N, "shiny gold"], BagRule).

find_shiny_gold(Rules, Rule, Rule) :-
    Rule = [_BagHead, BagRule],
    member([_N, NewBagHead], BagRule),
    member([NewBagHead, NewBagRule], Rules),
    find_shiny_gold(Rules, [NewBagHead, NewBagRule], _).

:- table bag_count/3.

bag_count(Rules, [BagNumber, BagRule], N) :-
    member([BagRule, []], Rules),
    N is 1*BagNumber.

bag_count(Rules, [BagNumber, BagRule], N) :-
    member([BagRule, BagBody], Rules),
    maplist(bag_count(Rules), BagBody, Count),
    sum_list(Count, N0),
    N is (N0+1)*BagNumber.

:- begin_tests(day7).

test(star1) :- star(1, 119),!.
test(star2) :- star(2, 155802),!.

:- end_tests(day7).