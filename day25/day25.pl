star(1, N) :-
    PublicKeyCard = 14222596,
    PublicKeyDoor = 4057428,
    SubjectNumber = 7,
    public_key(SubjectNumber, LoopSizeCard, PublicKeyCard),
    format('CARD - Subject Number: ~d - Loop size: ~d\n', [SubjectNumber, LoopSizeCard]),
    public_key(SubjectNumber, LoopSizeDoor, PublicKeyDoor),
    dif(LoopSizeCard, LoopSizeDoor),
    format('DOOR - Subject Number: ~d - Loop size: ~d\n', [SubjectNumber, LoopSizeDoor]),
    gen_public_key(PublicKeyCard, LoopSizeDoor, Key),
    gen_public_key(PublicKeyDoor, LoopSizeCard, Key),
    N = Key.

% SN = 18664
% CARD = 51742
% DOOR = 58660
% KEY? 18448032

public_key(SubjectNumber, LoopSize, PublicKey) :-
    public_key_(SubjectNumber, LoopSize, PublicKey, 1, SubjectNumber).

public_key_(_, _, _, 10000000, _) :- !,fail.

public_key_(SubjectNumber, LoopSize, PublicKey, Loops, Value) :-
    NewValue is (Value * SubjectNumber) mod 20201227,
    NewLoops is Loops + 1,
    (NewValue = PublicKey -> 
        LoopSize = NewLoops
    ;   (
        public_key_(SubjectNumber, LoopSize, PublicKey, NewLoops, NewValue)
    )
    ).

gen_public_key(SN, LoopSize, PublicKey) :-
    gen_public_key_(SN, LoopSize, PublicKey, 1, SN).

gen_public_key_(_, Loops, Value, Loops, Value).
gen_public_key_(SN, LoopSize, PublicKey, Loops, Value) :-
    NewValue is (Value * SN) mod 20201227,
    NewLoops is Loops + 1,
    gen_public_key_(SN, LoopSize, PublicKey, NewLoops, NewValue).
