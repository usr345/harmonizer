:- module(pitch_arithm2, [
              stage_pitch/2,
              note_sub/3,
              abs_pitch/2,
              interval_le/2,
              add_interval/3]).

:- use_module(library(clpfd)).

stage_pitch(0, 0).
stage_pitch(1, 2).
stage_pitch(2, 4).
stage_pitch(3, 5).
stage_pitch(4, 7).
stage_pitch(5, 9).
stage_pitch(6, 11).

abs_pitch(pitch(Octave, Stage, Alter), Pitch) :- stage_pitch(Stage, StagePitch), Pitch #= Octave * 12 + StagePitch + Alter.

note_sub(pitch(Octave1, Stage1, Alter1), pitch(Octave2, Stage2, Alter2), interval(Stage, Semitones)) :-
   abs_pitch(pitch(Octave1, Stage1, Alter1), Pitch1),
   abs_pitch(pitch(Octave2, Stage2, Alter2), Pitch2),
   Stage #= Octave1 * 7 + Stage1 - Octave2 * 7 - Stage2,
   Semitones #= Pitch1 - Pitch2.

add_interval(Note1, Interval, Note2) :- note_sub(Note2, Note1, Interval).

interval_less(interval(_, Semi1), interval(_, Semi2)) :- Semi1 #< Semi2.
interval_le(X, X).
interval_le(X, Y) :- interval_less(X, Y).
