:- module(day4, []).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).


input([[]]) --> eos.
input([[] | Data]) --> "\n", input(Data). 

input(NewData) -->
    string(Field),
    ":",
    string(Value),
    ( "\n" | " "),
    input(Data),
    {
        Data = [X|OldData],
        atom_string(AField, Field),
        atom_string(AValue, Value),
        Y =.. [AField, AValue],
        append(X, [Y], NewX),
        NewData = [NewX|OldData]
    }.

load_data(Passports) :-
    open('day4/input.dat', read, Stream),
    phrase_from_stream(input(Passports), Stream).

star(1, X) :-
    load_data(Passports),!,
    findall(Passport, (
        member(Passport, Passports),
        valid_passport(Passport)
    ), ValidPasswords),
    length(ValidPasswords, X).

star(2, X) :-
    load_data(Passports),!,
    findall(Passport, (
        member(Passport, Passports),
        valid_passport_2(Passport)
    ), ValidPasswords),
    length(ValidPasswords, X).

valid_passport(Passport) :-
    member(byr(_), Passport),
    member(iyr(_), Passport),
    member(eyr(_), Passport),
    member(hgt(_), Passport),
    member(hcl(_), Passport),
    member(ecl(_), Passport),
    member(pid(_), Passport).

hgt -->
    integer(X),
    "cm",
    {
        X >= 150,
        X =< 193
    }.

hgt --> 
    integer(X),
    "in",
    {
        X >= 59,
        X =< 76
    }.

hcl -->
    "#",
    xdigits(_).

valid_ecl(amb).
valid_ecl(blu).
valid_ecl(brn).
valid_ecl(gry).
valid_ecl(grn).
valid_ecl(hzl).
valid_ecl(oth).

valid_passport_2(Passport) :-
    member(byr(Byr), Passport),
    atom_number(Byr, NByr),
    NByr >= 1920,
    NByr =< 2002,
    member(iyr(Iyr), Passport),
    atom_number(Iyr, NIyr),
    NIyr >= 2010,
    NIyr =< 2020,
    member(eyr(Eyr), Passport),
    atom_number(Eyr, NEyr),
    NEyr >= 2020,
    NEyr =< 2030,
    member(hgt(Hgt), Passport),
    atom_codes(Hgt, HgtCodes),
    phrase(hgt, HgtCodes),
    member(hcl(Hcl), Passport),
    atom_codes(Hcl, HclCodes),
    phrase(hcl, HclCodes),
    member(ecl(Ecl), Passport),
    valid_ecl(Ecl),
    member(pid(Pid), Passport),
    atom_codes(Pid, PidCodes),
    length(PidCodes, 9),
    atom_number(Pid, _).

:- begin_tests(day4).

test(star1) :- star(1, 245), !.
test(star2) :- star(2, 133), !.

:- end_tests(day4).
