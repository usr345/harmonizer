:- use_module(library(clpfd)).
:- use_module(library(musicxml)).

stage_less(note(Octave1, _), note(Octave2, _)) :- Octave1 #< Octave2.
stage_less(note(Octave, Stage1), note(Octave, Stage2)) :- Stage1 #< Stage2.
stage_le(Stage1, Stage1).
stage_le(Stage1, Stage2) :- stage_less(Stage1, Stage2).

oct_up(note(Octave1, Stage), note(Octave2, Stage)) :- Octave2 #= Octave1 + 1.

nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)) :-  Octave2 #= Octave1 - 1, Stage1 #< Stage2.
nearest_down(note(Octave1, Stage), note(Octave2, Stage)) :-  Octave2 #= Octave1 - 1.
nearest_down(note(Octave, Stage1), note(Octave, Stage2)) :-  Stage1 #> Stage2.

nearests_down([], []).
nearests_down([NoteA|ATail], [NoteB|BTail]) :- nearest_down(NoteA, NoteB), nearests_down(ATail, BTail).

% Поиск следующей ноты в зацикленном списке
% параметры отношения:
% (нота, [список нот], следующая нота, первый элемент изначального списка)
xnext(N, [N, A | _], A, _).
xnext(N, [_ , X | M], A, F) :- xnext(N, [X | M], A, F).
% если ноту нашли последней, то она соседняя с первой
xnext(N, [N], A, A).

% Следующая нота в циклическом списке
rnext(N, [L | T], A) :- xnext(N, [L | T], A, L).

% По типу аккорда возвращает список нот
% ta - тоника
% da - доминанта
% sa - субдоминанта
chord_stages(ta, [1, 3, 5]).
chord_stages(da, [5, 7, 2]).
chord_stages(sa, [4, 6, 1]).

% ступень содержится в аккорде
is_in_chord(N, Chord) :- chord_stages(Chord, X), member(N, X).

% первая нота аккорда
% TODO: определить через chord_stages
chord_tonic(1, ta).
chord_tonic(5, da).
chord_tonic(4, sa).

% 2-я ступень
% wide/narrow - широкий\узкий аккорд
chord_third(UpperStage, ChordTonicStage, LowerStage, wide) :- chord_stages(ChordTonicStage, ChordStages), rnext(UpperStage, ChordStages, LowerStage).
chord_third(UpperStage, ChordTonicStage, LoweStage, narrow) :- chord_stages(ChordTonicStage, ChordStages), rnext(LoweStage, ChordStages, UpperStage).

% кусок бизнес-логики
%
harm1(Stage1, ChordTonicStage, Stage2, Stage3, Stage4, ChordArrangement) :- member(ChordTonicStage, [ta, sa, da]),
                                                                            member(ChordArrangement, [wide, narrow]),
                                                                            is_in_chord(Stage1, ChordTonicStage),
                                                                            chord_tonic(Stage4, ChordTonicStage),
                                                                            chord_third(Stage1, ChordTonicStage, Stage2, ChordArrangement),
                                                                            chord_third(Stage2, ChordTonicStage, Stage3, ChordArrangement).

% разрешенные последовательности аккордов
possible_next_chord(sa, ta).
possible_next_chord(sa, sa).
possible_next_chord(sa, da).
possible_next_chord(da, ta).
possible_next_chord(da, da).
possible_next_chord(ta, sa).
possible_next_chord(ta, da).
possible_next_chord(ta, ta).

harm_stages([N1, NN1 | NS1], [TDS, TDSN | TDSS], [N2, NN2 | NS2], [N3, NN3 | NS3], [N4, NN4 | NS4], [W, WN | WS]) :-
   harm1(N1, TDS, N2, N3, N4, W),
   possible_next_chord(TDS, TDSN),
   harm_stages([NN1 | NS1], [TDSN | TDSS], [NN2 | NS2], [NN3 | NS3], [NN4 | NS4], [WN | WS]).
harm_stages([N1], [TDS], [N2], [N3], [N4], [W]) :-
   harm1(N1, TDS, N2, N3, N4, W).

stages([], []).
stages([note(_, N) | T], [N | TS]) :- stages(T, TS).

nne(A, B) :- stage_less(A, B).
nne(A, B) :- stage_less(B, A).

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
dirs1(A1, B, _, A2, B, _) :- nne(A1, A2).
dirs1(A1, B1, C, A2, B2, C) :- nne(B1, B2), nne(A1, A2).

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
wnswitch([A, B | TS], [W, W | WS]) :- tne(A, B),
                                      wnswitch([B | TS], [W | WS]).

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

readMXML(File, XNotes, Alts) :-
       musicxml_score(File, element(_, _, S)),
       getElements(part, S, P),
       getElements(measure, P, M),
       getNotes(M, XNotes),
       altitudes(XNotes, Alts).
