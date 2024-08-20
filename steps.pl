%-*- mode: prolog-*-
:- module(steps, [scale1/2, scales/2, letter_num/2, note_semitone/2, build_scale/3, convert_scale/2, build_scale_human/3]).
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

%% большие интервалы - это интервалы, которые содержат два, четыре, девять или одиннадцать полутонов.
%% малые интервалы — это интервалы, содержащие один, три, восемь или десять полутонов.
%% чистые интервалы — это интервалы, содержащие 0, 5, 7 полутонов
% ч - чистый интервал
%% Уменьшенные интервалы - это интервалы, которые меньше, чем на один полутон от малого или чистого интервала.
%% Увеличенные интервалы — это интервалы, которые более чем на один полутон отстоят от большого или совершенного интервала.

% название, кол-во ступеней, кол-во полутонов, кол-во полутонов без учета октавы, кол-во ступеней чистое
intervals(prima(ч), 0, 0, 0, 0).
intervals(prima(ум), 0, 11, 11, 0).
intervals(prima(ув), 0, 1, 1, 0).
intervals(prima(дув), 0, 2, 2, 0).

% м.2
% б.2
intervals(secunda(ум), 1, 0, 0, 1).
intervals(secunda(м), 1, 1, 1, 1).
intervals(secunda(б), 1, 2, 2, 1).
intervals(secunda(ув), 1, 3, 3, 1).
% м.3
% б.3
intervals(tertia(ум), 2, 2, 2, 2).
intervals(tertia(м), 2, 3, 2, 2).
intervals(tertia(б), 2, 4, 2, 2).
intervals(tertia(ув), 2, 5, 2, 2).

intervals(quarta(ум), 3, 4, 4, 3).
intervals(quarta(ч), 3, 5, 5, 3).
intervals(quarta(ув), 3, 6, 6, 3).
intervals(quarta(дув), 3, 7, 7, 3).

intervals(quinta(дум), 4, 5, 5, 4).
intervals(quinta(ум), 4, 6, 6, 4).
intervals(quinta(ч), 4, 7, 7, 4).
intervals(quarta(ув), 4, 8, 8, 4).

intervals(sexta(ум), 5, 7, 7, 5).
intervals(sexta(м), 5, 8, 8, 5).
intervals(sexta(б), 5, 9, 9, 5).
intervals(sexta(ув), 5, 10, 10, 5).

intervals(septima(ум), 6, 9, 9, 6).
intervals(septima(м), 6, 10, 10, 6).
intervals(septima(б), 6, 11, 11, 6).
intervals(septima(ув), 6, 0, 12, 6).

intervals(octava(ум), 0, 11, 11, 7).
intervals(octava(ч), 0, 0, 12, 7).
intervals(octava(ув), 0, 1, 13, 7).

intervals(nona(ум), 1, 0, 12, 8).
intervals(nona(м), 1, 1, 13, 8).
intervals(nona(б), 1, 2, 14, 8).
intervals(nona(ув), 1, 3, 15, 8).

intervals(decima(ум), 2, 2, 14, 9).
intervals(decima(м), 2, 3, 15, 9).
intervals(decima(б), 2, 4, 16, 9).
intervals(decima(ув), 2, 5, 17, 9).

% по русскому названию интервала возвращает
% Name - его латинское название tertia(м), tertia(б)
% Steps2 - количество ступеней в интервале
% Semitones - кол-во полутонов в интервале
% steps:rus_intervals(int(м, 2), Name, Steps2, Semitones).
% Name = secunda(м),
% Steps2 = Semitones, Semitones = 1 .
rus_intervals(int(Type, Steps), Name, Steps2, Semitones) :- Steps #> 0,
                                                            Steps #< 11,
                                                            Steps1 #= Steps - 1,
                                                            intervals(Name, Steps2, Semitones, _, Steps1),
                                                            % =.. - превратить терм в список
                                                            Name =.. [_, Type].

same(X, X).
same(X, Y) :- X #= Y - 12.
same(X, Y) :- X #= Y + 12.

correct(X) :- X #>= 0, X #< 12.

same_step(X, X).
same_step(X, Y) :- X #= Y - 7.
same_step(X, Y) :- X #= Y + 7.

correct_step(X) :- X #>= 0, X #< 7.

letter_num('A', 0).
letter_num('B', 1).
letter_num('C', 2).
letter_num('D', 3).
letter_num('E', 4).
letter_num('F', 5).
letter_num('G', 6).

steps_diff(note(S1, _), note(S2, _), Result) :- letter_num(S1, N1),
                                                letter_num(S2, N2),
                                                Diff #= N2 - N1,
                                                same_step(Diff, Result),
                                                correct_step(Result).

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
% ?- build_scale(0, maj, Scale).
% Scale = [0, 2, 4, 5, 7, 9, 11] ;
build_scale(Semitone, Scale, Out) :- scales(Scale, Deltas),
                                     build_scale1(Semitone, Deltas, Out).

% Возвращает по начальному элементу список букв, составляющих шкалу:
% A - начальный элемент списка 'D' -> ['D', 'E', 'F', 'G', 'A', 'B', 'C']
% сначала мы находим букву в стандартном массиве, разбиваем его на 2 части:
% X - всё, что до буквы, и Y - всё, что после буквы
% Потом соединяем их в общий массив, получая шкалу, задаваемую данной буквой
letter_shift(A, Out) :- append([X, [A], Y], ['A', 'B', 'C', 'D', 'E', 'F', 'G']),
                        append([[A], Y, X], Out).


% C maj: ValsSemi = [0, 2, 4, 5, 7, 9, 11] -> множество возможных представлений в виде текстовых нот:
% [note('C', 0), note('C', 2), note('D', 2), note('D', 3), note('E', 3), note('C', -3), note('C', -1)]
convert_scale(ValsSemi, Out) :- maplist(note_semitone, Out, ValsSemi).

% Возвращает ноту без учета альтерации
get_note(note(X, _), X).

filter_scale(StartNote, Out) :- letter_shift(StartNote, Scale),
                                maplist(get_note, Out, Scale).

% Получить список нот, входящих в данную тональность
%% build_scale_human(note('G', 0), maj, Out).
%% Out = [note('G', 0), note('A', 0), note('B', 0), note('C', 0), note('D', 0), note('E', 0), note('F', 1)]
build_scale_human(note(X, Alt), Scale, Out) :- filter_scale(X, Out),
                                               note_semitone(note(X, Alt), Semitone),
                                               build_scale(Semitone, Scale, ValsSemi),
                                               convert_scale(ValsSemi, Out).

% Проверка того, что нота находится в тональности
% note(X, Alt) - проверяемая нота
% NoteT - тоника тональности
% Scale - наименование тональности
note_in_scale(X, NoteT, Scale) :-
    build_scale_human(NoteT, Scale, Out),
    member(X, Out).

% calc_intervals_numeric(note('C', 0), maj, 4, X, Y). - выдаст все пары нот, между которыми
% существует интервал 4 полутона
% Note - тоника
% Scale - лад
% Interval - кол-во полутонов
% Steps - количество ступеней, для которых существует заданный интервал
% intervals(quinta(ч), 4, 7).
calc_intervals_numeric(Note, Scale, Interval, Steps, Y, X) :- build_scale_human(Note, Scale, Out),

                                                              correct(Interval),
                                                              member(X, Out),
                                                              member(Y, Out),
                                                              note_semitone(X, XSemi),
                                                              note_semitone(Y, YSemi),
                                                              Delta #= XSemi - YSemi,
                                                              same(Delta, Interval),
                                                              steps_diff(Y, X, Steps).

calc_intervals(Note, Scale, Interval, Y, X) :-
    % название, кол-во ступеней, кол-во полутонов
    intervals(Interval, Steps, Semitones, _, _),
    calc_intervals_numeric(Note, Scale, Semitones, Steps, Y, X).

calc_intervals(Note, Scale, Interval, Y, X) :-
    % название, кол-во ступеней, кол-во полутонов
    rus_intervals(Interval, _, Steps, Semitones),
    calc_intervals_numeric(Note, Scale, Semitones, Steps, Y, X).


% Вычисляет все пары нот с заданным интервалом в тональности
% Note, Scale - входные переменные
% Interval, Y, X - выходные переменные
% Пример вызова:
% calc_all_intervals_numeric(note('C', 0), maj, Interval, X, Y).
calc_all_intervals_numeric(Note, Scale, Interval, Steps, Y, X) :- member(Interval, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]),
                                                                  calc_intervals_numeric(Note, Scale, Interval, Steps, Y, X).

neighbour('C', 'D').
neighbour('D', 'E').
neighbour('E', 'F').
neighbour('F', 'G').
neighbour('G', 'A').
neighbour('A', 'B').
neighbour('B', 'C').

tritons(Note, Scale, tri{note1: X, note2: Y}, solved_tri{note1: X1, note2: Y1}) :-

    % уменьшенная квинта
    calc_intervals_numeric(Note, Scale, 6, 4, X, Y),
    get_note(X, XL),
    get_note(Y, YL),
    neighbour(XL, NextXL),
    neighbour(PrevYL, YL),
    note_in_scale(X1, Note, Scale),
    note_in_scale(Y1, Note, Scale),
    X1 = note(NextXL, Alt1),
    Y1 = note(PrevYL, Alt2).

tritons(Note, Scale, tri{note1: X, note2: Y}, solved_tri{note1: X1, note2: Y1}) :-

    % увеличенная кварта
    calc_intervals_numeric(Note, Scale, 6, 3, X, Y),
    get_note(X, XL),
    get_note(Y, YL),
    neighbour(PrevXL, XL),
    neighbour(YL, NextYL),
    note_in_scale(X1, Note, Scale),
    note_in_scale(Y1, Note, Scale),
    X1 = note(PrevXL, Alt1),
    Y1 = note(NextYL, Alt2).
