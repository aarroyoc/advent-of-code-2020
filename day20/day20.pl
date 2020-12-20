:- module(day20, []).

:- set_prolog_flag(double_quotes, chars).

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

input([img(ID, Data)|Images]) -->
    "Tile ",
    integer(ID),
    ":\n",
    string_without("\n", Row1),"\n",
    string_without("\n", Row2),"\n",
    string_without("\n", Row3),"\n",
    string_without("\n", Row4),"\n",
    string_without("\n", Row5),"\n",
    string_without("\n", Row6),"\n",
    string_without("\n", Row7),"\n",
    string_without("\n", Row8),"\n",
    string_without("\n", Row9),"\n",
    string_without("\n", Row10),"\n",
    {
        Data = [Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9, Row10]
    },
    "\n",
    input(Images).

input([]) --> [].

load_data(Images) :-
    read_file_to_string('day20/input.dat', String, []),
    string_chars(String, Chars),
    phrase(input(Images), Chars).

star(1, N) :-
    load_data(Images),
    length(Table, 144),
    nlist(144, RNs),
    reverse(RNs, Ns),
    maplist(position(Images, Table), Ns, Table),
    nth1(1, Table, img(ID1, _)),
    nth1(12, Table, img(ID2, _)),
    nth1(133, Table, img(ID3, _)),
    nth1(144, Table, img(ID4, _)),
    N is ID1 * ID2 * ID3 * ID4.

star(2, N) :-
    load_data(Images),
    length(Table, 144),
    nlist(144, RNs),
    reverse(RNs, Ns),
    maplist(position(Images, Table), Ns, Table),!,
    join_tiles(Table, Image),
    findall(Monsters, (
        transform_image(img(0, Image), img(0, TImage)),
        monsters(TImage, Monsters)
    ), MonstersList),
    max_list(MonstersList, MonstersNumber),
    foldl(count_cells, Image, 0, Cells),
    N is Cells - MonstersNumber * 15.

count_cells(Row, AccIn, AccOut) :-
    foldl(count_cells_, Row, AccIn, AccOut).
count_cells_('#', In, Out) :-
    Out is In + 1.
count_cells_('.', In, In).

monsters(Image, N) :-
    findall(X, (
        between(1, 96, X),
        between(1, 96, Y),
        YTop is Y - 1,
        YDown is Y + 1,
        nth1(YTop, Image, RowT),
        nth1(Y, Image, RowM),
        nth1(YDown, Image, RowD),
        nth1(X, RowM, '#'),
        X2 is X + 5,
        nth1(X2, RowM, '#'),
        X3 is X + 6,
        nth1(X3, RowM, '#'),
        X4 is X3 + 5,
        nth1(X4, RowM, '#'),
        X5 is X3 + 6,
        nth1(X5, RowM, '#'),
        X6 is X5 + 5,
        nth1(X6, RowM, '#'),
        X7 is X5 + 6,
        nth1(X7, RowM, '#'),
        X8 is X5 + 7,
        nth1(X8, RowM, '#'),
        X9 is X + 18,
        nth1(X9, RowT, '#'),
        X10 is X + 1,
        nth1(X10, RowD, '#'),
        X11 is X + 4,
        nth1(X11, RowD, '#'),
        X12 is X + 7,
        nth1(X12, RowD, '#'),
        X13 is X + 10,
        nth1(X13, RowD, '#'),
        X14 is X + 13,
        nth1(X14, RowD, '#'),
        X15 is X + 16,
        nth1(X15, RowD, '#')
    ), Xs),
    length(Xs, N).

print_image(Image) :-
    forall(member(Row, Image), format("~w\n", [Row])).

join_tiles(Table, Image) :-
    maplist(remove_border, Table, NoBorderImage),
    join_rows(0, NoBorderImage, Image).

join_rows(96, _, []). % 96.
join_rows(N, NoBorderImage, [Row|Image]) :-
    X is N // 8,
    Y is N mod 8,
    X1 is X * 12,
    nth0(X1, NoBorderImage, Table1),
    nth0(Y, Table1, Row1),
    X2 is X*12 + 1,
    nth0(X2, NoBorderImage, Table2),
    nth0(Y, Table2, Row2),
    X3 is X*12 + 2,
    nth0(X3, NoBorderImage, Table3),
    nth0(Y, Table3, Row3),
    X4 is X*12 + 3,
    nth0(X4, NoBorderImage, Table4),
    nth0(Y, Table4, Row4),
    X5 is X*12 + 4,
    nth0(X5, NoBorderImage, Table5),
    nth0(Y, Table5, Row5),
    X6 is X*12 + 5,
    nth0(X6, NoBorderImage, Table6),
    nth0(Y, Table6, Row6),
    X7 is X*12 + 6,
    nth0(X7, NoBorderImage, Table7),
    nth0(Y, Table7, Row7),
    X8 is X*12 + 7,
    nth0(X8, NoBorderImage, Table8),
    nth0(Y, Table8, Row8),
    X9 is X*12 + 8,
    nth0(X9, NoBorderImage, Table9),
    nth0(Y, Table9, Row9),
    X10 is X*12 + 9,
    nth0(X10, NoBorderImage, Table10),
    nth0(Y, Table10, Row10),
    X11 is X*12 + 10,
    nth0(X11, NoBorderImage, Table11),
    nth0(Y, Table11, Row11),
    X12 is X*12 + 11,
    nth0(X12, NoBorderImage, Table12),
    nth0(Y, Table12, Row12),
    append(Row1, Row2, Row012),
    append(Row012, Row3, Row123),
    append(Row123, Row4, Row1234),
    append(Row1234, Row5, Row12345),
    append(Row12345, Row6, Row123456),
    append(Row123456, Row7, Row1234567),
    append(Row1234567, Row8, Row12345678),
    append(Row12345678, Row9, Row123456789),
    append(Row123456789, Row10, Row12345678910),
    append(Row12345678910, Row11, Row1234567891011),
    append(Row1234567891011, Row12, Row),
    NewN is N + 1,
    join_rows(NewN, NoBorderImage, Image).

remove_border(Tile, NoBorderTile) :-
    Tile = img(_, [_R1, R2, R3, R4, R5, R6, R7, R8, R9, _R10]),
    NoTopBottomTile = [R2, R3, R4, R5, R6, R7, R8, R9],
    maplist(remove_leftright, NoTopBottomTile, NoBorderTile).

remove_leftright(Row, NoBorderRow) :-
    Row = [_C1, C2, C3, C4, C5, C6, C7, C8, C9, _C10],
    NoBorderRow = [C2, C3, C4, C5, C6, C7, C8, C9].


position(Images, Table, N, TImage) :-
    member(Image, Images),
    Image = img(ID, _0),
    transform_image(Image, TImage),
    LeftN is N - 1,
    (
        (0 is LeftN mod 12) ->
            true
        ;   (
            nth1(LeftN, Table, Left),
            Left \= img(ID, _),
            left(TImage, LSide),
            right(Left, LSide)
        )
    ),
    UpN is N - 12,
    (
        (UpN < 1) ->
            true
        ;   (
            nth1(UpN, Table, Top),
            Top \= img(ID, _),
            top(TImage, TSide),
            bottom(Top, TSide)
        )
    ).

nlist(0, []).
nlist(N, [N|List]) :-
    X is N - 1,
    nlist(X, List).

write_table(Table) :-
    maplist(get_id, Table, Ids),
    write(Ids).

get_id(img(ID, _), ID).

transform_image(Image, Image).
transform_image(Image, TImage) :-
    flip(Image, TImage).
transform_image(Image, TImage) :-
    rotate(Image, TImage).
transform_image(Image, TImage) :-
    rotate(Image, TImage0),
    rotate(TImage0, TImage).
transform_image(Image, TImage) :-
    rotate(Image, TImage0),
    rotate(TImage0, TImage1),
    rotate(TImage1, TImage).
transform_image(Image, TImage) :-
    rotate(Image, TImage0),
    flip(TImage0, TImage).
transform_image(Image, TImage) :-
    rotate(Image, TImage0),
    rotate(TImage0, TImage1),
    flip(TImage1, TImage).
transform_image(Image, TImage) :-
    rotate(Image, TImage0),
    rotate(TImage0, TImage1),
    rotate(TImage1, TImage2),
    flip(TImage2, TImage).

flip(img(ID, Data), img(ID, FlippedData)) :-
    maplist(reverse, Data, FlippedData).

rotate(img(ID, Data), img(ID, RotatedData)) :-
    transpose(Data, Data0),
    maplist(reverse, Data0, RotatedData).

top(img(_, Data), Top) :-
    nth1(1, Data, Top).

bottom(img(_, Data), Bottom) :-
    nth1(10, Data, Bottom).

left(img(_, Data), Left) :-
    Left = [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10],
    Data = [
        [X1, _, _, _, _, _, _, _, _, _],
        [X2, _, _, _, _, _, _, _, _, _],
        [X3, _, _, _, _, _, _, _, _, _],
        [X4, _, _, _, _, _, _, _, _, _],
        [X5, _, _, _, _, _, _, _, _, _],
        [X6, _, _, _, _, _, _, _, _, _],
        [X7, _, _, _, _, _, _, _, _, _],
        [X8, _, _, _, _, _, _, _, _, _],
        [X9, _, _, _, _, _, _, _, _, _],
        [X10, _, _, _, _, _, _, _, _, _]
    ].

right(img(_, Data), Right) :-
    Right = [X1, X2, X3, X4, X5, X6, X7, X8, X9, X10],
    Data = [
        [_, _, _, _, _, _, _, _, _, X1],
        [_, _, _, _, _, _, _, _, _, X2],
        [_, _, _, _, _, _, _, _, _, X3],
        [_, _, _, _, _, _, _, _, _, X4],
        [_, _, _, _, _, _, _, _, _, X5],
        [_, _, _, _, _, _, _, _, _, X6],
        [_, _, _, _, _, _, _, _, _, X7],
        [_, _, _, _, _, _, _, _, _, X8],
        [_, _, _, _, _, _, _, _, _, X9],
        [_, _, _, _, _, _, _, _, _, X10]
    ].

:- begin_tests(day20).

test(star1) :- star(1, 108603771107737),!.
test(star2) :- star(2, 2129),!.

:- end_tests(day20).