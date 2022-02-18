%-*- mode: prolog-*-
:- use_module(library(clpfd)).

scale1('C', 0).
scale1('D', 2).
scale1('E', 4).
scale1('F', 5).
scale1('G', 7).
scale1('A', 9).
scale1('B', 11).

scales(maj, [2, 2, 1, 2, 2, 2]).
scales(maj_harm, [2, 2, 1, 2, 1, 3]).
scales(min, [2, 1, 2, 2, 1, 2]).
scales(min_harm, [2, 1, 2, 2, 1, 3]).

same(X, X).
same(X, Y) :- X #= Y - 12.
same(X, Y) :- X #= Y + 12.

correct(X) :- X #>= 0, X #< 12.

% Соответствие между нотой и его значением в полутонах
% note('D', 1) <-> 3
note_semitone(note(X, Alt), Semitones) :- correct(Semitones),
                                          scale1(X, Num),
                                          Y #= Semitones - Alt,
                                          same(Num, Y),
                                          Alt #> -4,
                                          Alt #< 4.

build_scale1(X, [], [X]).
%% [Y, | YS] - выходы
build_scale1(Semitone, [X | XS], [Semitone | YS]) :- Temp #= X + Semitone,
                                                     correct(Y),
                                                     same(Y, Temp),
                                                     build_scale1(Y, XS, YS).

% Начальный полутон и тональность (maj, min) -> [note(X, Alt)]
build_scale(Semitone, Scale, Out) :- scales(Scale, Deltas),
                                     build_scale1(Semitone, Deltas, Out).

% Возвращает по начальному элементу список букв, составляющих шкалу:
% A - начальный элемент списка 'D' -> ['D', 'E', 'F', 'G', 'A', 'B', 'C']
letter_shift(A, Out) :- append([X, [A], Y], ['A', 'B', 'C', 'D', 'E', 'F', 'G']), append([[A], Y, X], Out).


% C maj: ValsSemi = [0, 2, 4, 5, 7, 9, 11] -> множество возможных представлений в виде текстовых нот:
% [note('C', 0), note('C', 2), note('D', 2), note('D', 3), note('E', 3), note('C', -3), note('C', -1)]
convert_scale(ValsSemi, Out) :- maplist(note_semitone, Out, ValsSemi).

get_note(note(X, _), X).

filter_scale(StartNote, Out) :- letter_shift(StartNote, Val),
                                maplist(get_note, Out, Val).

build_scale_human(note(X, Alt), Scale, Out) :- filter_scale(X, Out),
                                               note_semitone(note(X, Alt), Semitone),
                                               build_scale(Semitone, Scale, ValsSemi),
                                               convert_scale(ValsSemi, Out).

% calc_intervals(note('C', 0), maj, 4, X, Y). - выдаст все пары нот, между которыми
% существует интервал 4
calc_intervals(Note, Scale, Interval, Y, X) :- build_scale_human(Note, Scale, Out),
                                               correct(Interval),
                                               member(X, Out),
                                               member(Y, Out),
                                               note_semitone(X, XSemi),
                                               note_semitone(Y, YSemi),
                                               Delta #= XSemi - YSemi,
                                               same(Delta, Interval).

% Вычисляет все пары нот с заданным интервалом в тональности
% Note, Scale - входные переменные
% Interval, Y, X - выходные переменные
% Пример вызова:
% calc_all_intervals(note('C', 0), maj, Interval, X, Y).
calc_all_intervals(Note, Scale, Interval, Y, X) :- member(Interval, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]),
                                                   calc_intervals(Note, Scale, Interval, Y, X).
