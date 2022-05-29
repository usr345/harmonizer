%-*- mode: prolog-*-
:- use_module(library(clpfd)).
:- use_module(steps).

intervals(dim, [[3, 6], [3, 9], [6, 9]]).
intervals(min, [[3, 7], [4, 9], [5, 8]]).
intervals(maj, [[4, 7], [3, 8], [5, 9]]).
intervals(aug, [[4, 8], [4, 8], [4, 8]]).

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
    intervals(Type, IntervalsList),
    nth0(Inversion, IntervalsList, Intervals),
    maplist(plus(Abs), Intervals, Chord1),
    maplist(mod12, Chord1, Chord2),
    notes_nums(X, N_0),
    % вычисляем дельты для нот
    notes_intervals(Inversion, NotesIntevals),
    nth0(0, NotesIntevals, I1),
    nth0(1, NotesIntevals, I2),
    N_1 #= mod(N_0 + I1, 7),
    N_2 #= mod(N_0 + I2, 7),
    notes_nums(X_1, N_1),
    notes_nums(X_2, N_2),
    [C1, C2] = Chord2,
    note2abs(note(X_1, Alt1), C1),
    note2abs(note(X_2, Alt2), C2),
    Chord = [note(X, Alt), note(X_1, Alt1), note(X_2, Alt2)].

allChords3(Note, Out) :-
    chord3(Note, dim, 0, Chord1),
    chord3(Note, dim, 1, Chord2),
    chord3(Note, dim, 2, Chord3),
    chord3(Note, min, 0, Chord4),
    chord3(Note, min, 1, Chord5),
    chord3(Note, min, 2, Chord6),
    chord3(Note, maj, 0, Chord7),
    chord3(Note, maj, 1, Chord8),
    chord3(Note, maj, 2, Chord9),
    chord3(Note, aug, 0, Chord10),
    chord3(Note, aug, 1, Chord11),
    chord3(Note, aug, 2, Chord12),
    append([Chord1, Chord2, Chord3, Chord4, Chord5, Chord6, Chord7, Chord8, Chord9, Chord10, Chord11, Chord12], [], Out),
    write(Out).
