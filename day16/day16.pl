:- module(day16, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).


input_fields([field(FieldName, [range(X1, X2), range(Y1, Y2)], _Position)|Fields]) -->
    string_without(":", FieldName),
    ": ",
    integer(X1),
    "-",
    integer(X2),
    " or ",
    integer(Y1),
    "-",
    integer(Y2),
    "\n",
    input_fields(Fields).

input_fields([]) --> [].

ticket([X|Ticket]) -->
    integer(X),
    ",",
    ticket(Ticket).

ticket([X]) -->
    integer(X),
    "\n".

input_tickets([Ticket|Tickets]) -->
    ticket(Ticket),
    input_tickets(Tickets).

input_tickets([]) --> [].

input(Fields, MyTicket, Tickets) -->
    input_fields(Fields),
    "\nyour ticket:\n",
    ticket(MyTicket),
    "\nnearby tickets:\n",
    input_tickets(Tickets).

load_data(Fields, MyTicket, Tickets) :-
    read_file_to_string('day16/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Fields, MyTicket, Tickets), Chars).

star(1, N) :-
    load_data(Fields, _MyTicket, Tickets),
    findall(Field, (
        member(Ticket, Tickets),
        member(Field, Ticket),
        \+ ticket_field(Field, Fields)
    ), InvalidFields),
    sum_list(InvalidFields, N).

star(2, N) :-
    load_data(Fields, MyTicket, Tickets),
    include(all_fields_valid(Fields), Tickets, ValidTickets),
    length(MyTicket, Length),
    maplist(get_all_positions(Length, ValidTickets), Fields, FieldsPositions),
    select_field(Fields, FieldsPositions),
    foldl(product_departure(MyTicket), Fields, 1, N).

select_field(Fields, FieldsPositions) :-
    member(Positions, FieldsPositions),
    length(Positions, 1),
    nth1(Index, FieldsPositions, Positions),
    nth1(Index, Fields, Field),
    Positions = [Position],
    Field = field(_, _, Position),
    maplist(remove_position(Position), FieldsPositions, NewFieldsPositions),
    select_field(Fields, NewFieldsPositions).

select_field(_, FieldsPositions) :-
    forall(member(Positions, FieldsPositions),(length(Positions, 0))).

remove_position(Position, Field, NewField) :-
    delete(Field, Position, NewField).

all_fields_valid(Fields, Ticket) :-
    forall(member(Field, Ticket), ticket_field(Field, Fields)).

get_all_positions(Length, Tickets, Field, Positions) :-
    findall(Position,(
        field_position(Length, Tickets, Field),
        Field = field(_, _, Position)
    ), Positions).

field_position(Length, Tickets, field(Name, [range(X1, X2), range(Y1, Y2)], Position)) :-
    between(1, Length, Position),
    forall(member(Ticket, Tickets),(
        nth1(Position, Ticket, N),
        (between(X1, X2, N);between(Y1, Y2, N))
    )).

get_position(field(_, _, Position), X, [Position|X]).

product_departure(Ticket, field(Name, _, Position), AccIn, AccOut) :-
    (
        append("departure", _, Name) ->
            nth1(Position, Ticket, X)
        ;   X is 1
    ),
    AccOut is AccIn * X.


ticket_field(Field, Fields) :-
    member(field(_, Ranges, _), Fields),
    member(range(X1, X2), Ranges),
    between(X1, X2, Field).

:- begin_tests(day16).

test(star1) :- star(1, 20231), !.
test(star2) :- star(2, 1940065747861),!.

:- end_tests(day16).