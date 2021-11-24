%-*- mode: prolog-*-

:- use_module(pitch_arithm).
:- use_module(utility).
:- use_module(read).
:- use_module(library(clpfd)).

% Запрет на ложное перекрещивание голосов
block_intersection_pair(N11, N12, N21, N22) :-
    stage_le(N12, N21), stage_le(N22, N11).

block_intersection(N11, N12, N13, N14, N21, N22, N23, N24) :-
    block_intersection_pair(N11, N12, N21, N22),
    block_intersection_pair(N12, N13, N22, N23),
    block_intersection_pair(N13, N14, N23, N24).

block_intersection_arr([_], [_], [_], [_]).
block_intersection_arr([N11, N21 | N1s], [N12, N22 | N2s], [N13, N23 | N3s], [N14, N24 | N4s]) :-
    block_intersection(N11, N12, N13, N14, N21, N22, N23, N24),
    block_intersection_arr([N21 | N1s], [N22 | N2s], [N23 | N3s], [N24 | N4s]).


% разрешенные названия аккордов
chord_types([ta, da, sa]).
% По типу аккорда возвращает список ступеней аккорда: прима, терция, квинта
% ta - тоника
% da - доминанта
% sa - субдоминанта
chord_stages(ta, [1, 3, 5]).
chord_stages(da, [5, 7, 2]).
chord_stages(sa, [4, 6, 1]).

% ступень содержится в аккорде
is_in_chord(N, Chord) :- chord_stages(Chord, X), member(N, X).

% Прима аккорда
chord_primo(X, Chord) :- chord_stages(Chord, [X | _]).

% соседние ноты в аккорде
% UpperStage: верхняя нота
% ChordType: {ta, da, sa}
% LowerStage: нижняя нота
% wide/narrow - широкое\тесное расположение
chord_neighbours(UpperStage, ChordType, LowerStage, wide) :- chord_stages(ChordType, ChordStages), rnext(UpperStage, ChordStages, LowerStage).
chord_neighbours(UpperStage, ChordType, LowerStage, narrow) :- chord_stages(ChordType, ChordStages), rnext(LowerStage, ChordStages, UpperStage).

test1([note(1, 1), note(1, 2)], [note(3, 5), note(3, 6)], [note(3, 4), note(3,6)], [note(3, 7), note(3,1)]).

both_eq(X, X, Y, Y).

paroct([_], [_]).
paroct([note(_, S1), N1 | T1], [note(_, S2), N2 | T2]) :-
   note(_, X1) = N1,
   note(_, X2) = N2,
   both_eq(S1, S2, X1, X2),
   paroct([N1 | T1], [N2 | T2]).

parocts(A) :-
   append([_, [X], _, [Y], _], A),
   paroct(X, Y).

both_q(X1, X2, Y1, Y2) :- mod(X1 - X2, 7) #= 4, mod(Y1 - Y2, 7) #= 4.

parq([_], [_]).
parq([note(_, S1), N1 | T1], [note(_, S2), N2 | T2]) :-
   note(_, X1) = N1,
   note(_, X2) = N2,
   both_q(S1, S2, X1, X2),
   paroct([N1 | T1], [N2 | T2]).

parq(A) :-
   append([_, [X], _, [Y], _], A),
   parq(X, Y).


% Гармонизация 4-х ступеней по одной известной ступени
% Stage1: int \in [1, 7] - верхняя нота (номер ступени)
% ChordType: {ta, da, sa} - тип аккорда
% Stage2: int \in [1, 7]
% Stage3: int \in [1, 7]
% Stage4: int \in [1, 7] - бас
% ChordArrangement: {wide, narrow}
harm1(Stage1, ChordType, Stage2, Stage3, Stage4, ChordArrangement) :-
    chord_types(AllowedChordTypes),
    member(ChordType, AllowedChordTypes),
    member(ChordArrangement, [wide, narrow]),
    % поиск типа аккорда по заданной ступени
    is_in_chord(Stage1, ChordType),
    chord_primo(Stage4, ChordType),
    % вычисляем следующие ступени
    chord_neighbours(Stage1, ChordType, Stage2, ChordArrangement),
    chord_neighbours(Stage2, ChordType, Stage3, ChordArrangement).


% разрешенные последовательности аккордов
% После T и S может быть всё, что угодно.
possible_next_chord(X, Y) :- member(X, [ta, sa]),
                             chord_types(AllowedChordTypes),
                             member(Y, AllowedChordTypes).

% запрещен ход D - S
possible_next_chord(da, Y) :- member(Y, [da, ta]).

% Гармонизация списков нот, аккордов и расположений без учета октав.
% По заданному списку нот (как правило, это будет либо [N1] - сопрано,
% либо - [N4] - бас) возвращает списки нот оставшихся 3-х голосов,
% типы аккордов, и их расположение.
harm_stages([N1], [TDS], [N2], [N3], [N4], [W]) :-
   harm1(N1, TDS, N2, N3, N4, W).

harm_stages([N1, NN1 | NS1], [TDS, TDSN | TDSS], [N2, NN2 | NS2], [N3, NN3 | NS3], [N4, NN4 | NS4], [W, WN | WS]) :-
    % поиск первого аккорда
    harm1(N1, TDS, N2, N3, N4, W),
    possible_next_chord(TDS, TDSN),
    harm_stages([NN1 | NS1], [TDSN | TDSS], [NN2 | NS2], [NN3 | NS3], [NN4 | NS4], [WN | WS]).

stages([], []).
stages([note(_, N) | T], [N | TS]) :- stages(T, TS).

% Нам даны 2 массива с нотами, соответствующие 2-м соседним аккордам.
% Каждый массив содержит 4 ноты, соответствующие 4-м голосам от
% верхнего к нижнему.
% Предикат проверяет, что не все голоса идут в одну сторону и не все одинаковы.
%% not_all([X | T]) :- \+ maplist(==(X), T).
%% not_all([X1, X2]) :- X1 #\= X2.
not_all([X1, X2 | _]) :- X1 #\= X2.
not_all([X, X | T]) :- not_all([X | T]).
not_all_one([N1 | T1], [N2 | T2]) :- maplist(notes_cmp, [N1 | T1], [N2 | T2], Deltas),
                                     not_all(Deltas).


% Аккорды не равны
tne(da, ta).
tne(da, sa).
tne(sa, ta).
tne(sa, da).
tne(ta, da).
tne(ta, sa).

% прямое перемещение в рамках одного аккорда.
% Сохраняются тип аккорда, расположение и бас.
one_chord(note(Octave1, Stage1), note(Octave2, Stage2), note(Octave3, Stage3), note(Octave4, Stage4), ChordType, ChordArrangement, note(Octave1N, Stage1N), note(Octave1N, Stage1N), note(Octave1N, Stage1N), note(Octave4, Stage4)) :-
    harm1(Stage1, ChordType, Stage2, Stage3, Stage4, ChordArrangement),
    nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)),
    nearest_down(note(Octave2, Stage2), note(Octave3, Stage3)),
    nearest_down_bass(note(Octave3, Stage4), note(Octave4, Stage4)).
    %% harm1(Stage1N, ChordType, Stage2N, Stage3N, Stage4, ChordArrangement),


wnswitch([_], [_]).
wnswitch([A, A | TS], [wide, narrow | WS]) :- wnswitch([A | TS], [narrow | WS]).
wnswitch([A, A | TS], [narrow, wide | WS]) :- wnswitch([A | TS], [wide | WS]).
wnswitch([_, B | TS], [W, W | WS]) :- wnswitch([B | TS], [W | WS]).

wnswitch_n1([_], [_]).
wnswitch_n1([_, N2 | T], [W, W | WS]) :- wnswitch_n1([N2 | T], [W | WS]).
wnswitch_n1([N1, N2 | T], [narrow, wide | WS]) :- stage_less(N1, N2),
                                                 wnswitch_n1([N2 | T], [wide | WS]).
wnswitch_n1([N1, N2 | T], [wide, narrow | WS]) :- stage_less(N2, N1),
                                                 wnswitch_n1([N2 | T], [narrow | WS]).


same_length([], []).
same_length([_|A], [_|B]) :- same_length(A,B).

dirs([_], [_], [_], [_]).
dirs([A1, A2 | AS], [B1, B2 | BS], [C1, C2 | CS], [D1, D2 | DS]) :-
    not_all_one([A1, B1, C1, D1], [A2, B2, C2, D2]),
    dirs([A2 | AS], [B2 | BS], [C2 | CS], [D2 | DS]).

check_downbeat_step([_], [_], _).

check_downbeat_step([ChordType1, ChordType2 | ChordTypes], [W2 | Ws], _) :-
    tne(ChordType1, ChordType2), check_downbeat_step([ChordType2 | ChordTypes], Ws, W2).

check_downbeat_step([ChordType, ChordType | ChordTypes], [W2 | Ws], MaxW) :-
    W2 #< MaxW, check_downbeat_step([ChordType | ChordTypes], Ws, MaxW).

% unused
check_downbeat(X, [W | Ws]) :- check_downbeat_step(X, Ws, W).

check_measures([_], [_]).

check_measures([ChordType1, ChordType2 | ChordTypes], [_, start | Ss]) :-
    tne(ChordType1, ChordType2), check_measures([ChordType2 | ChordTypes], [start | Ss]).

check_measures([_, X | ChordTypes], [_, non_start | Ss]) :-
    check_measures([X | ChordTypes], [non_start | Ss]).

%% bass_normal(X, Y, Y, Z) :- right(X, Y), .

% Гармонизация списков нот в формате note(Октава, Ступень)
harm(N1, TDS, N2, N3, N4, W, Strength, Measures) :-
   same_length(N1, TDS),
   same_length(N1, N2),
   same_length(N1, N3),
   same_length(N1, N4),
   same_length(N1, W),
   % выбираем ноту из пары (октава, нота), игнорируя октаву
   stages(N1, NN1),
   stages(N2, NN2),
   stages(N3, NN3),
   stages(N4, NN4),
   % без учета октавы
   harm_stages(NN1, TDS, NN2, NN3, NN4, W),
   wnswitch(TDS, W),
   wnswitch_n1(N1, W),
   nearests_down(N1, N2),
   nearests_down(N2, N3),
   nearests_down_bass(N3, N4),
   notes_less_oct_arr(N4),
   block_intersection_arr(N1, N2, N3, N4),
   dirs(N1, N2, N3, N4),
   %% check_downbeat(TDS, Strength).
   check_measures(TDS, Measures),
   \+ parocts([N1, N2, N3, N4]),
   \+ parq([N1, N2, N3, N4]).

%% music(test, [note(5, 5), note(5, 6), note(5, 5), note(5, 3), note(5, 4), note(5, 2), note(5, 1)], [2, 1, 2, 1, 2, 1, 2], [1, 0, 1, 0, 1, 0, 1]).
music(test, [note(5, 5), note(5, 6), note(5, 5), note(5, 3), note(5, 4), note(5, 2), note(5, 1)], [2, 1, 2, 1, 2, 1, 2], [start, non_start, start, non_start, start, non_start, start]).

force([_,_], [2, 1]).
force([_,_,_], [3, 2, 1]).
force([_,_,_,_], [4, 2, 3, 1]).
force([_,_,_,_,_,_], [5, 4, 3, 2, 1]).
force([_,_,_,_,_,_,_], [6, 5, 4, 3, 2, 1]).
force([_,_,_,_,_,_,_,_], [7, 6, 5, 4, 3, 2, 1]).
force([_,_,_,_,_,_,_,_,_], [8, 7, 6, 5, 4, 3, 2, 1]).
force([_,_,_,_,_,_,_,_,_,_], [9, 8, 7, 6, 5, 4, 3, 2, 1]).

harmFile(File, S, T, N1, N2, N3, N4, C, W) :-
     readNotes(File, S, T, N1, Marks),
     harm(N1, C, N2, N3, N4, W, _, Marks).
