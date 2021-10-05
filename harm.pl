%-*- mode: prolog-*-
:- use_module(library(clpfd)).
:- use_module(library(musicxml)).

stage_less(note(Octave1, _), note(Octave2, _)) :- Octave1 #< Octave2.
stage_less(note(Octave, Stage1), note(Octave, Stage2)) :- Stage1 #< Stage2.
stage_le(Stage1, Stage1).
stage_le(Stage1, Stage2) :- stage_less(Stage1, Stage2).

notes_ne(note(Octave1, _), note(Octave2, _)) :- Octave1 #\= Octave2.
notes_ne(note(Octave, Stage1), note(Octave, Stage2)) :- Stage1 #\= Stage2.

oct_up(note(Octave1, Stage), note(Octave2, Stage)) :- Octave2 #= Octave1 + 1.

% у нас есть нота
% по ноте и другой ноте, у которой не задана октава, он подбирает октаву так, чтобы он была ближе всего
nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)) :-  Octave2 #= Octave1 - 1, Stage1 #< Stage2.
nearest_down(note(Octave1, Stage), note(Octave2, Stage)) :-  Octave2 #= Octave1 - 1.
nearest_down(note(Octave, Stage1), note(Octave, Stage2)) :-  Stage1 #> Stage2.

nearests_down([], []).
nearests_down([NoteA|ATail], [NoteB|BTail]) :- nearest_down(NoteA, NoteB), nearests_down(ATail, BTail).

% данное отношение является вспомогательным для rnext
% Поиск следующего элемента в зацикленном списке
% параметры отношения:
% (элемент, [список], следующий элемент, первый элемент изначального списка)
xnext(N, [N, A | _], A, _).
xnext(N, [_ , X | M], A, F) :- xnext(N, [X | M], A, F).
% Последний элемент соседний с первым
xnext(N, [N], A, A).


% Следующий элемент в циклическом списке
rnext(N, [H | T], A) :- xnext(N, [H | T], A, H).

% разрешенные названия аккордов
chord_types([ta, da, sa]).
% По типу аккорда возвращает список нот
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

% Гармонизация 4-х нот по типу аккорда, расположению (широкий, тесный) и одной известной ноте
% Stage1 - верхняя нота
% ChordType - тип аккорда
% Stage2
% Stage3
% Stage4 - бас
% ChordArrangement: {wide, narrow}
harm1(Stage1, ChordType, Stage2, Stage3, Stage4, ChordArrangement) :-
    chord_types(AllowedChordTypes),
    member(ChordType, AllowedChordTypes),
    member(ChordArrangement, [wide, narrow]),
    is_in_chord(Stage1, ChordType),
    chord_primo(Stage4, ChordType),
    chord_neighbours(Stage1, ChordType, Stage2, ChordArrangement),
    chord_neighbours(Stage2, ChordType, Stage3, ChordArrangement).

% разрешенные последовательности аккордов
% После T и S может быть всё, что угодно.
possible_next_chord(X, Y) :- member(X, [ta, sa]),
                             chord_types(AllowedChordTypes),
                             member(Y, AllowedChordTypes).

% запрещен ход D - S
possible_next_chord(da, Y) :- member(Y, [da, ta]).

% Гармонизация списков нот, аккордов и расположений.
% По заданному списку нот. Как правило, это будет либо [N1] - сопрано,
% либо - [N4] - бас.
% Возвращает списки нот оставшихся 3-х голосов, типы аккордов, и их расположение.
harm_stages([N1], [TDS], [N2], [N3], [N4], [W]) :-
   harm1(N1, TDS, N2, N3, N4, W).

harm_stages([N1, NN1 | NS1], [TDS, TDSN | TDSS], [N2, NN2 | NS2], [N3, NN3 | NS3], [N4, NN4 | NS4], [W, WN | WS]) :-
   harm1(N1, TDS, N2, N3, N4, W),
   possible_next_chord(TDS, TDSN),
   harm_stages([NN1 | NS1], [TDSN | TDSS], [NN2 | NS2], [NN3 | NS3], [NN4 | NS4], [WN | WS]).

stages([], []).
stages([note(_, N) | T], [N | TS]) :- stages(T, TS).

% Пусть нам даны 2 голоса, ноты с октавами.
% Мы берем 2 соседних аккорда, и проверяем, что ноты не идут в одну сторону.
% В одну сторону означает, что N1_2 < N1_1 & N2_2 < N2_2
% N1_1 - 1-й голос 1-я нота
% N1_2 - 1-й голос 2-я нота
% N2_1 - 2-й голос 1-я нота
% N2_2 - 2-й голос 2-я нота
not_one(N, N, _, _).
not_one(_, _, N, N).
not_one(N1_1, N1_2, N2_1, N2_2) :- stage_less(N1_1, N1_2), not(stage_less(N2_1, N2_2)).
not_one(N1_1, N1_2, N2_1, N2_2) :- not(stage_less(N1_1, N1_2)), stage_less(N2_1, N2_2).

% У нас есть 2 массива нот, отображающие 2 последовательных аккорда с нотами для каждого голоса
% Предикат возвращает true, если существует
sub_sign(A, B, C) :- C is sign(A - B).
%% maplist(sub_sign,[1,2,3],[3,2,1],C).
not_all([X|T]) :- \+ maplist(==(X), T).
%% maplist(sub_sign,[1,2,3],[4,5,5],C), not_all(C).


% все голоса не могут идти в одну сторону
dirs1(A1, B1, C1, A2, B2, C2) :-
   stage_less(A1, A2),
   stage_less(B1, B2),
   stage_less(C2, C1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   stage_less(A1, A2),
   stage_less(C1, C2),
   stage_less(B2, B1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   stage_less(C1, C2),
   stage_less(B1, B2),
   stage_less(A2, A1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   stage_less(A2, A1),
   stage_less(B2, B1),
   stage_less(C1, C2).
dirs1(A1, B1, C1, A2, B2, C2) :-
   stage_less(A2, A1),
   stage_less(B1, B2),
   stage_less(C2, C1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   stage_less(A1, A2),
   stage_less(B2, B1),
   stage_less(C2, C1).
dirs1(A, _, _, A, _, _).
dirs1(A1, B, _, A2, B, _) :- notes_ne(A1, A2).
dirs1(A1, B1, C, A2, B2, C) :- notes_ne(B1, B2), notes_ne(A1, A2).

dirs([_], [_], [_]).
dirs([A1, A2 | AS], [B1, B2 | BS], [C1, C2 | CS]) :-
   dirs([A2|AS], [B2|BS], [C2|CS]),
   dirs1(A1, B1, C1, A2, B2, C2).

tne(da, ta).
tne(da, sa).
tne(sa, ta).
tne(sa, da).
tne(ta, da).
tne(ta, sa).


wnswitch([_], [_]).
wnswitch([A, A | TS], [wide, narrow | WS]) :- wnswitch([A | TS], [narrow | WS]).
wnswitch([A, A | TS], [narrow, wide | WS]) :- wnswitch([A | TS], [wide | WS]).
wnswitch([_, B | TS], [W, W | WS]) :- wnswitch([B | TS], [W | WS]).

same_length([], []).
same_length([_|A], [_|B]) :- same_length(A,B).

harm(N1, TDS, N2, N3, N4, W) :-
   same_length(N1, TDS),
   same_length(N1, N2),
   same_length(N1, N3),
   same_length(N1, N4),
   same_length(N1, W),
   stages(N1, NN1),
   stages(N2, NN2),
   stages(N3, NN3),
   stages(N4, NN4),
   harm_stages(NN1, TDS, NN2, NN3, NN4, W),
   wnswitch(TDS, W),
   nearests_down(N1, N2),
   nearests_down(N2, N3),
   nearests_down(N3, N4),
   dirs(N2, N3, N4).


% чтение файла

getAlter(MNote, Alter) :-
       member(element(alter, _, [AlterChar]), MNote), atom_number(AlterChar, Alter), !.
getAlter(_, 0).

getNote(MNote, xnote(Octave, StepChar, Alter)) :-
       member(element(step, _, [StepChar]), MNote),
       member(element(octave, _, [OctaveChar]), MNote),
       atom_number(OctaveChar, Octave),
       getAlter(MNote, Alter).

getNotes([], []).
getNotes([element(note, _, [element(pitch, _, MNote) | _])|Tail], [XNote|OTail]) :-
       getNote(MNote, XNote),
       getNotes(Tail, OTail), !.
getNotes([_|Tail], OTail) :- getNotes(Tail, OTail).

getElements(_, [], []).
getElements(E, [element(E, _, M) | T], MO) :- getElements(E, T, MT), append(M, MT, MO), !.
getElements(E, [_|T], MO) :- getElements(E, T, MO).

shift('C', 0).
shift('D', 2).
shift('E', 4).
shift('F', 5).
shift('G', 7).
shift('A', 9).
shift('B', 11).

altitude(xnote(O, N, A), H) :- shift(N, D), H #= (O * 12) + D + A.
altitudes([], []).
altitudes([A|AS], [O|OS]) :- altitude(A, O), altitudes(AS, OS).

altitude2note(S, T, A, note(O, N)) :-
	X #= mod(A - S, 12),
	member(stage(X, N), T),
	O #= div(A - S, 12).
altitudes2notes(_, _, [], []).
altitudes2notes(S, T, [A|AS], [N|NS]) :- altitude2note(S, T, A, N), altitudes2notes(S, T, AS, NS).

tons(maj, [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)]).

readMXML(File, XNotes, Alts) :-
       musicxml_score(File, element(_, _, S)),
       getElements(part, S, P),
       getElements(measure, P, M),
       getNotes(M, XNotes),
       altitudes(XNotes, Alts).

readNotes(File, S, T, Notes) :-
       readMXML(File, _, Alts),
       tons(T, L),
       altitudes2notes(S, L, Alts, Notes).

harmFile(File, S, T, N1, N2, N3, N4, C, W) :-
     readNotes(File, S, T, N1),
     harm(N1, C, N2, N3, N4, W).
