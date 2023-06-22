%-*- mode: prolog -*-
:- use_module(library(clpfd)).
:- use_module(steps).

% Таблица (название типа, номер обращения, кол-во полутонов для вычисления следующей ноты)
% второе число - это сумма следующего интервала с предыдущим.
%% intervals(dim, 0, [3, 6]).
%% intervals(dim, 1, [3, 9]).
%% intervals(dim, 2, [6, 9]).
%% intervals(min, 0, [3, 7]).
%% intervals(min, 1, [4, 9]).
%% intervals(min, 2, [5, 8]).
%% intervals(maj, 0, [4, 7]).
%% intervals(maj, 1, [3, 8]).
%% intervals(maj, 2, [5, 9]).
%% intervals(aug, 0, [4, 8]).
%% intervals(aug, 1, [4, 8]).
%% intervals(aug, 2, [4, 8]).
size_to_number(small, 3).
size_to_number(big, 4).

intervals(dim, [3, 3]).
intervals(min, [3, 4]).
intervals(maj, [4, 3]).
intervals(aug, [4, 4]).

intervals(sept(X, Size), Out) :-
    size_to_number(Size, N),
    intervals(X, Result),
    append(Result, [N], Out).

% это дельты соотв. типа трезвучия для шкалы из 7 нот
% C D E F G A B
% 0 1 2 3 4 5 6
% |   |   |
% 2 - расстояние от C до E
% 4 - расстояние от C до G
notes_intervals(0, [2, 4]).
notes_intervals(1, [2, 5]).
notes_intervals(2, [3, 5]).

sum_elements([], 0).
sum_elements([X | Tail], Out) :-
    sum_elements(Tail, Result),
    Out #= X + Result.

shift(0, X, X).
shift(N, [X1 | Tail], Out) :-
    N #\= 0,
    sum_elements([X1 | Tail], Sum),
    Last #= 12 - Sum,
    append(Tail, [Last], Next),
    M is N - 1,
    shift(M, Next, Out).

accum_intervals([], []).
accum_intervals([X | Tail], [X | Out]) :-
    accum_intervals(Tail, Result),
    maplist(plus(X), Result, Out).

notes_intervals4(0, [2, 4, 6]).
notes_intervals4(1, [2, 4, 5]).
notes_intervals4(2, [2, 3, 5]).
notes_intervals4(3, [1, 3, 5]).


mod12(In, Out) :-
    Out #= mod(In, 12).

pred1(X, Out, Alt) :-
    Out #< X,
    Alt #< 0.

pred1(X, Out, Alt) :-
    Out #> X,
    Alt #> 0.

pred1(X, X, 0).

note2abs(note(X, Alt), Out) :-
    scale1(X, Val),
    Temp #= Val + Alt,
    Out #= mod(Temp, 12),
    Temp #< 25,
    Temp #> -1,
    Alt #< 11,
    Alt #> -11,
    pred1(Val, Out, Alt).

notes_nums('C', 0).
notes_nums('D', 1).
notes_nums('E', 2).
notes_nums('F', 3).
notes_nums('G', 4).
notes_nums('A', 5).
notes_nums('B', 6).

% Вычисляет ноты, входящие в аккорд без учета тональности
% Note = note(X, Alt)
% Type - тип аккорда: {dim, min, maj, aug}
% Inversion - номер обращения (0, 1, 2)
chord3(note(X, Alt), Type, Inversion, Chord) :-
    Inversion #< 4,
    note2abs(note(X, Alt), Abs),
    % intervals - это таблица интервалов для аккорда соотв. типа
    intervals(Type, Deltas),
    shift(Inversion, Deltas, DeltasInverted),
    accum_intervals(DeltasInverted, Intervals),
    maplist(plus(Abs), Intervals, Chord1),
    maplist(mod12, Chord1, Chord2),
    notes_nums(X, N_0),
    % вычисляем дельты для нот
    notes_intervals(Inversion, [I1, I2]),
    % номера 2-й и 3-й нот аккорда в семиэлеметной шкале
    N_1 #= mod(N_0 + I1, 7),
    N_2 #= mod(N_0 + I2, 7),
    % преобразование из чисел в буквы
    notes_nums(X_1, N_1),
    notes_nums(X_2, N_2),
    % унификация Chord2
    [C1, C2] = Chord2,
    % Зная буквы для X_1 и X_2, вычисляем альтерации
    note2abs(note(X_1, Alt1), C1),
    note2abs(note(X_2, Alt2), C2),
    Chord = [note(X, Alt), note(X_1, Alt1), note(X_2, Alt2)].

% Вычисляет ноты, входящие в аккорд без учета тональности
% Note = note(X, Alt)
% Type - тип аккорда: {dim, min, maj, aug}
% Inversion - номер обращения (0, 1, 2)
chord4(note(X, Alt), Type, Inversion, Chord) :-
    Inversion #< 5,
    note2abs(note(X, Alt), Abs),
    % intervals - это таблица интервалов для аккорда соотв. типа
    intervals(Type, Deltas),
    shift(Inversion, Deltas, DeltasInverted),
    accum_intervals(DeltasInverted, Intervals),
    maplist(plus(Abs), Intervals, Chord1),
    maplist(mod12, Chord1, Chord2),
    notes_nums(X, N_0),
    % вычисляем дельты для нот
    notes_intervals4(Inversion, [I1, I2, I3]),
    % номера 2-й и 3-й нот аккорда в семиэлеметной шкале
    N_1 #= mod(N_0 + I1, 7),
    N_2 #= mod(N_0 + I2, 7),
    N_3 #= mod(N_0 + I3, 7),
    % преобразование из чисел в буквы
    notes_nums(X_1, N_1),
    notes_nums(X_2, N_2),
    notes_nums(X_3, N_3),
    % унификация Chord2
    [C1, C2, C3] = Chord2,
    % Зная буквы для X_1 и X_2, вычисляем альтерации
    note2abs(note(X_1, Alt1), C1),
    note2abs(note(X_2, Alt2), C2),
    note2abs(note(X_3, Alt3), C3),
    Chord = [note(X, Alt), note(X_1, Alt1), note(X_2, Alt2), note(X_3, Alt3)].

lc([], []).
lc([H|T], [E|O]) :- lc(T, O), member(E, H).

chord(Note, cord{chord: Chord, type: ChordType, inversion: Inversion}) :-
    member(ChordType, [dim, min, maj, aug]),
    member(Inversion, [0, 1, 2]),
    chord3(Note, ChordType, Inversion, Chord).

printChordsByTypes(_, []).
printChordsByTypes(Note, [CT|ChordTypes]) :-
    writeln(CT),
    printChordsByInversions(Note, CT, [0, 1, 2]),
    write("\n"),
    printChordsByTypes(Note, ChordTypes).

printChordsByInversions(_, _, []).
printChordsByInversions(Note, CT, [I|IS]) :-
    chord(Note, _{type: CT, inversion: I, chord: C}),
    writeln(C),
    printChordsByInversions(Note, CT, IS).

printChords4ByInversions(_, _, []).
printChords4ByInversions(Note, CT, [Inv|IS]) :-
    chord4(Note, CT, Inv, Chord),
    writeln(Chord),
    printChords4ByInversions(Note, CT, IS).

printChords4ByTypes(_, []).
printChords4ByTypes(Note, [CT|ChordTypes]) :-
    writeln(CT),
    printChords4ByInversions(Note, CT, [0, 1, 2, 3]),
    write("\n"),
    printChords4ByTypes(Note, ChordTypes).

printChords(Note) :- printChordsByTypes(Note, [dim, min, maj, aug]).
printChords4(Note) :- printChords4ByTypes(Note, [sept(dim, small), sept(dim, big), sept(min, small), sept(min, big), sept(maj, small), sept(maj, big), sept(aug, small), sept(aug, big)]).
