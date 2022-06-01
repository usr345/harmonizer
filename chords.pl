%-*- mode: prolog -*-
:- use_module(library(clpfd)).
:- use_module(steps).

intervals(dim, 0, [3, 6]).
intervals(dim, 1, [3, 9]).
intervals(dim, 2, [6, 9]).
intervals(min, 0, [3, 7]).
intervals(min, 1, [4, 9]).
intervals(min, 2, [5, 8]).
intervals(maj, 0, [4, 7]).
intervals(maj, 1, [3, 8]).
intervals(maj, 2, [5, 9]).
intervals(aug, 0, [4, 8]).
intervals(aug, 1, [4, 8]).
intervals(aug, 2, [4, 8]).

notes_intervals(0, [2, 4]).
notes_intervals(1, [2, 5]).
notes_intervals(2, [3, 5]).

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
    mod12(Temp, Out),
    Temp #< 25,
    Temp #> -1,
    Alt #< 11,
    Alt #> -11.
    %pred1(Val, Out, Alt).

notes_nums('C', 0).
notes_nums('D', 1).
notes_nums('E', 2).
notes_nums('F', 3).
notes_nums('G', 4).
notes_nums('A', 5).
notes_nums('B', 6).

permut(List, List, 0).
permut([X | T], List, N) :-
    N #> 0,
    append(T, [X], List1),
    N1 #= N - 1,
    permut(List1, List, N1).

% Вычисляет ноты, входящие в аккорд без учета тональности
% None = note(X, Alt)
% Type - тип аккорда: {dim, min, maj, aug}
% Inversion - номер обращения (0, 1, 2)
chord3(note(X, Alt), Type, Inversion, Chord) :-
    Inversion #< 4,
    note2abs(note(X, Alt), Abs),
    intervals(Type, Inversion, Intervals),
    maplist(plus(Abs), Intervals, Chord1),
    maplist(mod12, Chord1, Chord2),
    notes_nums(X, N_0),
    % вычисляем дельты для нот
    notes_intervals(Inversion, [I1, I2]),
    N_1 #= mod(N_0 + I1, 7),
    N_2 #= mod(N_0 + I2, 7),
    notes_nums(X_1, N_1),
    notes_nums(X_2, N_2),
    [C1, C2] = Chord2,
    note2abs(note(X_1, Alt1), C1),
    note2abs(note(X_2, Alt2), C2),
    Chord = [note(X, Alt), note(X_1, Alt1), note(X_2, Alt2)].

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

printChords(Note) :- printChordsByTypes(Note, [dim, min, maj, aug]).
