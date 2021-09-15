%-*- mode: prolog-*-
:- use_module(library(clpfd)).

note_less(note(A, _), note(B, _)) :- A #< B.
note_less(note(A, C), note(A, D)) :- C #< D.
note_le(A, A).
note_le(A, B) :- nole_less(A, B).

oct_up(note(A, B), note(C, B)) :- C #= A + 1.

nearest(note(A, B), note(C, D)) :-  C #= A - 1, B #< D.
nearest(note(A, B), note(C, B)) :-  C #= A - 1.
nearest(note(A, B), note(A, D)) :-  B #> D.

nearests([], []).
nearests([A|AS], [B|BS]) :- nearest(A, B), nearests(AS, BS).

% принадлежит ли элемент списку
in_list(N, [N|_]).
in_list(N, [_|M]) :- in_list(N, M).

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
dall(ta, [1, 3, 5]).
dall(da, [5, 7, 2]).
dall(sa, [4, 6, 1]).

% нота содержится в аккорде
tds(N, ta) :- in_list(N, [1, 3, 5]).
tds(N, da) :- in_list(N, [5, 7, 2]).
tds(N, sa) :- in_list(N, [4, 6, 1]).

% первая нота аккорда
first(1, ta).
first(5, da).
first(4, sa).

% 2-я нота
% N1 - верхняя нота
% TDS - тип аккорда
% N2 - нижняя нота
% wide/narrow - тип аккорда
n2(N1, TDS, N2, wide) :- dall(TDS, ARR), rnext(N1, ARR, N2).
n2(N1, TDS, N2, narrow) :- dall(TDS, ARR), rnext(N2, ARR, N1).

harm1(N1, TDS, N2, N3, N4, W) :- in_list(TDS, [ta, sa, da]), in_list(W, [wide, narrow]), tds(N1, TDS), first(N4, TDS), n2(N1, TDS, N2, W), n2(N2, TDS, N3, W).

% разрешенные последовательности аккордов
nexttds(sa, ta).
nexttds(sa, sa).
nexttds(sa, da).
nexttds(da, ta).
nexttds(da, da).
nexttds(ta, sa).
nexttds(ta, da).
nexttds(ta, ta).

harm_stages([N1, NN1 | NS1], [TDS, TDSN | TDSS], [N2, NN2 | NS2], [N3, NN3 | NS3], [N4, NN4 | NS4], [W, WN | WS]) :-
   harm1(N1, TDS, N2, N3, N4, W),
   nexttds(TDS, TDSN),
   harm_stages([NN1 | NS1], [TDSN | TDSS], [NN2 | NS2], [NN3 | NS3], [NN4 | NS4], [WN | WS]).
harm_stages([N1], [TDS], [N2], [N3], [N4], [W]) :-
   harm1(N1, TDS, N2, N3, N4, W).

stages([], []).
stages([note(_, N) | T], [N | TS]) :- stages(T, TS).

nne(A, B) :- note_less(A, B).
nne(A, B) :- note_less(B, A).

dirs1(A1, B1, C1, A2, B2, C2) :-
   note_less(A1, A2),
   note_less(B1, B2),
   note_less(C2, C1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   note_less(A1, A2),
   note_less(C1, C2),
   note_less(B2, B1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   note_less(C1, C2),
   note_less(B1, B2),
   note_less(A2, A1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   note_less(A2, A1),
   note_less(B2, B1),
   note_less(C1, C2).
dirs1(A1, B1, C1, A2, B2, C2) :-
   note_less(A2, A1),
   note_less(B1, B2),
   note_less(C2, C1).
dirs1(A1, B1, C1, A2, B2, C2) :-
   note_less(A1, A2),
   note_less(B2, B1),
   note_less(C2, C1).
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
   nearests(N1, N2),
   nearests(N2, N3),
   nearests(N3, N4),
   dirs(N2, N3, N4).
