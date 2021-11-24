%-*- mode: prolog-*-
:- module(pitch_arithm, [
	stage_le/2, stage_less/2,
	nearest_down/2, nearest_down_bass/2,
	nearests_down/2, nearests_down_bass/2,
	altitudes/2,
	notes_cmp/3,
	notes_less_oct_arr/1,
	tons/2, altitudes2notes/4]).
:- use_module(library(clpfd)).

stage_less(note(Octave1, _), note(Octave2, _)) :- Octave1 #< Octave2.
stage_less(note(Octave, Stage1), note(Octave, Stage2)) :- Stage1 #< Stage2.

stage_le(note(Octave, Stage), note(Octave, Stage)).
stage_le(Stage1, Stage2) :- stage_less(Stage1, Stage2).

stages_eq(note(Octave, Stage), note(Octave, Stage)).

% unused
stage_to_abs(note(Octave, Stage), X) :- X #= Octave*7 + Stage.

% unused
stages_ne(note(Octave1, _), note(Octave2, _)) :- Octave1 #\= Octave2.
stages_ne(note(Octave, Stage1), note(Octave, Stage2)) :- Stage1 #\= Stage2.

notes_cmp(X, Y, 0) :- stages_eq(X, Y).
notes_cmp(X, Y, -1) :- stage_less(X, Y).
notes_cmp(X, Y, 1) :- stage_less(Y, X).

less_then_oct(note(Octave, Stage), note(Octave, Stage)).
less_then_oct(note(Octave1, Stage1), note(Octave2, Stage2)) :- nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)).
less_then_oct(note(Octave1, Stage1), note(Octave2, Stage2)) :- nearest_down(note(Octave2, Stage2), note(Octave1, Stage1)).

notes_less_oct_arr([_]).
notes_less_oct_arr([N1, N2 | T]) :- less_then_oct(N1, N2), notes_less_oct_arr([N2 | T]).

% по ноте и другой ноте, у которой не задана октава, подбирает октаву так, чтобы он была ближе всего
% предполагается, что нота2 лежит ниже ноты1 в рамках одного аккорда
nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)) :-  Octave2 #= Octave1 - 1, Stage1 #< Stage2.
nearest_down(note(Octave1, Stage), note(Octave2, Stage)) :-  Octave2 #= Octave1 - 1.
nearest_down(note(Octave, Stage1), note(Octave, Stage2)) :-  Stage1 #> Stage2.

nearests_down([], []).
nearests_down([NoteA|ATail], [NoteB|BTail]) :- nearest_down(NoteA, NoteB), nearests_down(ATail, BTail).

% поиск ближайшей нижней ноты с учетом возможности ухода баса вниз на 2 октавы
nearest_down_bass(Bass, Bass).

nearest_down_bass(note(Octave1, Stage1), note(Octave2, Stage2)) :-
    nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)).

nearest_down_bass(note(Octave1, Stage1), note(Octave2_down, Stage2)) :-
    nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)),
    Octave2_down #= Octave2 - 1.

nearests_down_bass([], []).
nearests_down_bass([NoteA|ATail], [NoteB|BTail]) :- nearest_down_bass(NoteA, NoteB), nearests_down_bass(ATail, BTail).

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

/*
Конвертация абсолютной величины в ноту
- S - абсолютная величина тоники - от 0 до 11
- T - описание тональности: ступень, абсолютный номер ноты [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)]
- A - абсолютное значение ноты с учетом октавы
- note(O, N) - октава, ступень лада
*/
altitude2note(S, T, A, note(O, N)) :-
	X #= mod(A - S, 12),
	member(stage(X, N), T),
	O #= div(A - S, 12).
altitudes2notes(_, _, [], []).
altitudes2notes(S, T, [A|AS], [N|NS]) :- altitude2note(S, T, A, N), altitudes2notes(S, T, AS, NS).

% соответствие между номером мажорной ступени в хроматической гамме
% и номером ступени в мажорной тональности
tons(maj, [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)]).
tons(min, [stage(0,1), stage(2,2), stage(3, 3), stage(5, 4), stage(7, 5), stage(8, 6), stage(11, 7)]).
