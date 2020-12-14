:- module(day14, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

input([]) --> eos.
input(Steps) -->
    "mask = ",
    string_without("\n", Mask),
    "\n",
    input_mem(Mem),
    input(Steps0), {
        S1 = [mask(Mask)|Mem],
        S = [S1|Steps0],
        flatten(S, Steps)
    }.

input_mem([mem(Address, Value)|Mems]) -->
    "mem[",
    integer(Address),
    "] = ",
    integer(Value),
    "\n",
    input_mem(Mems).

input_mem([]) --> [].

load_data(Steps) :-
    read_file_to_string('day14/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Steps), Chars).

star(1, N) :-
    load_data(Steps),!,
    apply_steps(Steps, [], MemOut),
    foldl(sum_values, MemOut, 0, N).

star(2, N) :-
    load_data(Steps),!,
    apply_steps2(Steps, [], MemOut),
    foldl(sum_values, MemOut, 0, N).

apply_steps([], _, []).
apply_steps([mask(Mask)|Mems], _, MemOut) :-
    reverse(Mask, ReverseMask),
    apply_steps(Mems, ReverseMask, MemOut).
apply_steps([mem(Address, Value)|Mems], Mask, MemOut) :-
    integer_to_bits(Value, Bits),
    mask(Bits, Mask, MaskedValue),
    bits_to_integer(MaskedValue, FinalValue),
    apply_steps(Mems, Mask, MemOut0),!,
    (member(mem(Address, _), MemOut0) -> MemOut = MemOut0; MemOut = [mem(Address, FinalValue)|MemOut0]).

apply_steps2([], _, []).
apply_steps2([mask(Mask)|Mems], _, MemOut) :-
    reverse(Mask, ReverseMask),
    apply_steps2(Mems, ReverseMask, MemOut).
apply_steps2([mem(Address, Value)|Mems], Mask, MemOut) :-
    integer_to_bits(Address, Bits),!,
    apply_steps2(Mems, Mask, MemOut1),
    findall(Mem, (
        mask2(Bits, Mask, BitsMaskedAddress),
        bits_to_integer(BitsMaskedAddress, MaskedAddress),
        Mem = mem(MaskedAddress, Value),
        \+ member(mem(MaskedAddress, _), MemOut1)
    ), MemOut0),
    append(MemOut0, MemOut1, MemOut).

sum_values(mem(_, Value), AccIn, AccOut) :-
    AccOut is AccIn + Value.

integer_to_bits(0, [0]) :- !.
integer_to_bits(1, [1]) :- !.
integer_to_bits(Int, [Bit|Bits]) :-
    Bit is Int mod 2,
    IntRest is Int // 2,
    integer_to_bits(IntRest, Bits).

bits_to_integer(Bits, Out) :- !,bits_to_integer_(0, Bits, Out).
bits_to_integer_(_, [], 0).
bits_to_integer_(N, [Bit|Bits], Out) :-
    NewN is N + 1,
    bits_to_integer_(NewN, Bits, Out0),!,
    Out is Out0 + Bit*2^N.

mask([], [], []).
mask([], ['X'|Masks], [Value|Values]) :-
    Value = 0,
    mask([], Masks, Values).

mask([], ['0'|Masks], [Value|Values]) :-
    Value = 0,
    mask([], Masks, Values).

mask([], ['1'|Masks], [Value|Values]) :-
    Value = 1,
    mask([], Masks, Values).

mask([Bit|Bits], ['X'|Masks], [Value|Values]) :-
    Bit = Value,
    mask(Bits, Masks, Values).

mask([_Bit|Bits], ['1'|Masks], [Value|Values]) :-
    Value = 1,
    mask(Bits, Masks, Values).

mask([_Bit|Bits], ['0'|Masks], [Value|Values]) :-
    Value = 0,
    mask(Bits, Masks, Values).

mask2([], [], []).
mask2([], ['X'|Masks], [Value|Values]) :-
    (Value = 0;Value = 1),
    mask2([], Masks, Values).

mask2([], ['0'|Masks], [Value|Values]) :-
    Value = 0,
    mask2([], Masks, Values).

mask2([], ['1'|Masks], [Value|Values]) :-
    Value = 1,
    mask2([], Masks, Values).

mask2([_Bit|Bits], ['X'|Masks], [Value|Values]) :-
    (Value = 0;Value = 1),
    mask2(Bits, Masks, Values).

mask2([_Bit|Bits], ['1'|Masks], [Value|Values]) :-
    Value = 1,
    mask2(Bits, Masks, Values).

mask2([Bit|Bits], ['0'|Masks], [Value|Values]) :-
    Value = Bit,
    mask2(Bits, Masks, Values).

:- begin_tests(day14).

test(star1) :- star(1, 11926135976176),!.
test(star2) :- star(2, 4330547254348),!.

:- end_tests(day14).