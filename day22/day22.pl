:- module(day22, []).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(stack_limit, 4_294_967_296).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(ordsets)).

input_deck([Card|Deck]) -->
    integer(Card),
    "\n",
    input_deck(Deck).

input_deck([]) --> [].

input(Deck1, Deck2) -->
    "Player 1:\n",
    input_deck(Deck1),
    "\nPlayer 2:\n",
    input_deck(Deck2).

load_data(Deck1, Deck2) :-
    read_file_to_string('day22/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Deck1, Deck2), Chars).

star(1, N) :-
    load_data(Deck1, Deck2),
    game(Deck1, Deck2, FinalDeck),
    score(FinalDeck, N).

star(2, N) :-
    load_data(Deck1, Deck2),
    game2(Deck1, Deck2, FinalDeck1, FinalDeck2, []),
    append(FinalDeck1, FinalDeck2, FinalDeck),
    score(FinalDeck, N).

game2(Deck1, Deck2, FinalDeck1, FinalDeck2, Games) :-
    (ord_memberchk(Deck1-Deck2, Games) ->
        (
            FinalDeck1 = Deck1,
            FinalDeck2 = []
        );(
            ord_add_element(Games, Deck1-Deck2, NewGames),
            game2_(Deck1, Deck2, FinalDeck1, FinalDeck2, NewGames)
    )).

game2_([], Deck2, [], Deck2, _) :- \+ length(Deck2, 0).
game2_(Deck1, [], Deck1, [], _) :- \+ length(Deck1, 0).


game2_([Card1|Deck1], [Card2|Deck2], FinalDeck1, FinalDeck2, Games) :-
    length(SubDeck1, Card1),
    length(SubDeck2, Card2),
    append(SubDeck1, _, Deck1),
    append(SubDeck2, _, Deck2),
    game2(SubDeck1, SubDeck2, FinalDeck01, _FinalDeck02, []),!,
    (
        FinalDeck01 = [] ->
            (append(Deck2, [Card2, Card1], NewDeck2), game2(Deck1, NewDeck2, FinalDeck1, FinalDeck2, Games))
        ;   (append(Deck1, [Card1, Card2], NewDeck1), game2(NewDeck1, Deck2, FinalDeck1, FinalDeck2, Games))
    ).

game2_([Card1|Deck1], [Card2|Deck2], FinalDeck1, FinalDeck2, Games) :-
    Card1 > Card2,
    append(Deck1, [Card1, Card2], NewDeck1),
    game2(NewDeck1, Deck2, FinalDeck1, FinalDeck2, Games).

game2_([Card1|Deck1], [Card2|Deck2], FinalDeck1, FinalDeck2, Games) :-
    Card1 < Card2,
    append(Deck2, [Card2, Card1], NewDeck2),
    game2(Deck1, NewDeck2, FinalDeck1, FinalDeck2, Games).

game([], Deck2, Deck2) :- \+ length(Deck2, 0).
game(Deck1, [], Deck1) :- \+ length(Deck1, 0).

game([Card1|Deck1], [Card2|Deck2], FinalDeck) :-
    Card1 > Card2,
    append(Deck1, [Card1, Card2], NewDeck1),
    game(NewDeck1, Deck2, FinalDeck).

game([Card1|Deck1], [Card2|Deck2], FinalDeck) :-
    Card1 < Card2,
    append(Deck2, [Card2, Card1], NewDeck2),
    game(Deck1, NewDeck2, FinalDeck).

score([], 0).
score([Card|Deck], Score) :-
    score(Deck, Score0),
    length(Deck, N),
    Score is Score0 + (N+1)*Card.

:- begin_tests(day22).

test(star1) :- star(1, 33400),!.
test(star2) :- star(2, 33745),!.

:- end_tests(day22).