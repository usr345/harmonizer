%-*- mode: prolog-*-
:- module(tests, [
	test/4, run_test/1
	]).

:- use_module(pitch_arithm).
:- use_module(harm).
:- use_module(utility).
% discontiguous - Informs the system that the clauses of the specified predicate(s) might not be together in the source file.
:- discontiguous(run_test/1).
:- discontiguous(test/4).
:- discontiguous(test_harm/1).

run_test(Test) :- test(Test, Params, Pred/_, positive),
                  apply(Pred, Params).

run_test(Test) :- test(Test, Params, Pred/_, negative),
                  \+ apply(Pred, Params).

run_test(Test) :- test(Test, Params, Pred/_, unique),
		  % First - это все элементы массива Params, кроме последнего элемента
                  append(First, [Val], Params),
                  % вызываем Pred со всеми параметрами, кроме последнего
                  % в него записываем переменную X
                  % Потом сравниваем, что результат в X равен Val.
                  findall(X, (append(First, [X], P), apply(Pred, P)), [Val]).

run_test(Test) :- test(Test, Params, Pred/_, multiset),
                  append(First, [Val], Params),
                  findall(X, (append(First, [X], P), apply(Pred, P)), Res),
		  same_elements(Res, Val).


run_test_pred(Pred) :-
    forall(test(Test, _, Pred/_, _), run_test(Test)).

test(test1, [note(1, 5), note(3, 5)], stage_less/2, positive).
test(test2, [note(1, 5), note(1, 6)], stage_less/2, positive).
test(test3, [note(2, 5), note(1, 5)], stage_less/2, negative).
test(test4, [note(1, 5), note(1, 5)], stage_less/2, negative).
test(test5, [note(1, 5), note(3, 5)], stage_le/2, positive).
test(test6, [note(1, 5), note(1, 6)], stage_le/2, positive).
test(test7, [note(1, 6), note(1, 6)], stage_le/2, positive).
test(test8, [note(3, 5), note(1, 2)], stage_le/2, negative).

% notes_cmp
test(test9, [note(1, 1), note(1, 3), -1], notes_cmp/3, unique).
test(test10, [note(2, 5), note(2, 3), 1], notes_cmp/3, unique).
test(test11, [note(3, 3), note(5, 3), -1], notes_cmp/3, unique).
test(test23, [note(3, 2), note(3, 2), 0], notes_cmp/3, unique).
test(test24, [note(3, 5), note(1, 5), 1], notes_cmp/3, unique).

% less_then_oct
test(test12, [note(5, 3), note(4, 3)], less_then_oct/2, positive).
test(test13, [note(2, 3), note(2, 3)], less_then_oct/2, positive).
test(test14, [note(5, 3), note(5, 2)], less_then_oct/2, positive).
test(test15, [note(5, 3), note(4, 2)], less_then_oct/2, negative).
test(test16, [note(5, 3), note(2, 2)], less_then_oct/2, negative).
test(test14_1, [[note(5, 3), note(5, 2), note(5, 2)]], notes_less_oct_arr/1, positive).
test(test16_1, [[note(5, 3), note(2, 2)]], notes_less_oct_arr/1, negative).
test(test16_2, [[note(4, 7), note(5, 3), note(6, 4)]], notes_less_oct_arr/1, negative).

% задается нота 1, и по ней ищется нота 2, которая лежит ниже
test(test17, [note(2, 1), note(1, 5)], nearest_down/2, positive).
test(test18, [note(2, 3), note(1, 3)], nearest_down/2, positive).
test(test19, [note(2, 5), note(2, 3)], nearest_down/2, positive).

test(test19_1, [[note(2, 5), note(2, 3)], [note(1, 6), note(2, 1)]], nearests_down/2, positive).
test(test19_2, [[note(2, 5), note(2, 3)], [note(1, 4), note(2, 3)]], nearests_down/2, negative).
test(test19_3, [[note(2, 5), note(2, 3)], [note(1, 6), note(3, 4)]], nearests_down/2, negative).

nearest_down_bass(N, Stage, X) :- nearest_down_bass(N, note(X, Stage)).

% нота, [список разрешенных октав], ступень
test(test20, [note(5, 3), 1, [5, 4]], nearest_down_bass/2, multiset).
test(test21, [note(5, 3), 3, [5, 4, 3]], nearest_down_bass/2, multiset).

% altitude
test(test21(1), [xnote(0, 'C', 0), 0], altitude/2, unique).
test(test21(2), [xnote(0, 'C', 1), 1], altitude/2, unique).
test(test21(3), [xnote(0, 'D', -1), 1], altitude/2, unique).
test(test21(4), [xnote(2, 'C', 0), 24], altitude/2, unique).
test(test21(5), [xnote(1, 'B', 1), 24], altitude/2, unique).
test(test21(6), [xnote(1, 'G', -2), 17], altitude/2, unique).

test(test21a, [In, Out], altitudes/2, unique) :-
    findall(X, test(_, X, altitude/2, unique), List),
    maplist(nth0(0), List, In),
    maplist(nth0(1), List, Out).

same_elements([], []).
same_elements([X | XS], Y) :- append([A, [X], B], Y), % в Y-е встречается X?
                              append(A, B, Z),
                              same_elements(XS, Z), !.

test(test22, [[a, b, c, a], [c, a, b, a]], same_elements/2, positive).
test(test22_1, [[a, b, c, a], [c, a, a, b]], same_elements/2, positive).
test(test22_2, [[], []], same_elements/2, positive).
test(test22_3, [[a, b, c, a], [c, a, b]], same_elements/2, negative).
test(test22_4, [[a, b, c, a, d], [c, a, a, b]], same_elements/2, negative).
test(test22_5, [[a], []], same_elements/2, negative).

% 7 - тоника в G
% далее идет мажорный лад лад
% 12 - абсолютная величина ноты C 1-й октавы. В тональности G-maj 4 ступень 0-й октавы
test(test25(1), [7, [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)], 12, note(0, 4)], altitude2note/4, unique).
test(test25(2), [7, [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)], 6, note(-1, 7)], altitude2note/4, unique).

test(test26, [In1, In2, In3, Out], altitudes2notes/4, unique) :-
    findall(Y, test(_, Y, altitude2note/4, unique), List1),
    member([In1, In2 | _], List1),
    writeln(In1),
    writeln(In2),
    findall(X, test(_, [In1, In2 | X], altitude2note/4, unique), List),
    maplist(nth0(0), List, In3),
    writeln(In3),
    maplist(nth0(1), List, Out).


%% Предикаты, на которые есть тесты
%predicates_tested(Module, Set) :- findall(P, (module_property(Module, exports(X)), member(P, X), test(_, _, P, _)), List),
%                                  list_to_set(List, Set).
%
%predicates_not_tested(Module, Set) :- findall(P, (module_property(Module, exports(X)), member(P, X), \+ test(_, _, P, _)), List),
%                                  list_to_set(List, Set).

% Запрет на перенос одного типа аккорда со слабой доли на сильную
test(test27, [[ta, sa, da, da, ta, da, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, positive).
test(test28, [[ta, ta, ta, ta, ta, ta, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, positive).
test(test29, [[ta, sa, sa, da, ta, da, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, negative).
test(test30, [[ta, sa, da, da, da, ta, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, negative).

%% nearest_down_bass

%% test_harm_example(melody1, [[note(5, 3), note(5, 1)], [note(4, 5), note(4, 3)], [note(4, 1), note(3, 5)], [note(3, 1), note(3, 1)], [ta, ta], [wide, wide], [start, non_start], [2, 1]]).

% wide / narrow
test_harm_example(melody2, [[note(5, 3), note(5, 1)], [note(4, 5), note(4, 5)], [note(4, 1), note(4, 3)], [note(3, 1), note(3, 1)], [ta, ta], [wide, narrow], [start, non_start], [2, 1]]).

%% test_harm_neg_example(melody3, [[note(5, 3), note(5, 1)], [note(4, 5), note(4, 3)], [note(4, 1), note(3, 5)], [note(3, 1), note(3, 1)], [ta, ta], [wide, narrow], [start, non_start], [2, 1]]).

% Предикат harm получает входные параметры, необходимые для гармонизации, и возвращает
% списки голосов, которые получились в результате гармонизации.
test_harm(Test) :- test_harm_example(Test, [N1, N2, N3, N4, Types, Widths, Measures, Strengths]),
                   %% parq([N1, N2, N3, N4]).
                   harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).

test_harm_example(melody3, [note(5, 5), note(5, 6), note(5, 5), note(5, 3), note(5, 4), note(5, 2), note(5, 1)], [2, 1, 2, 1, 2, 1, 2], [start, non_start, start, non_start, start, non_start, start]).

% A-min
test_harm_example(melody4,
                  [
                  [note(5, 5), note(5, 6), note(5, 5), note(5, 3)],%, note(5, 4), note(5, 2), note(5, 1)],
                  [note(5, 3), note(5, 4), note(5, 2), note(5, 1), note(5, 1), note(4, 7), note(4, 5)],
                  [note(5, 1), note(5, 1), note(4, 7), note(4, 5), note(4, 6), note(4, 5), note(4, 3)],
                  [note(4, 1), note(3, 4), note(3, 5), note(4, 1), note(3, 4), note(3, 5), note(4, 1)],
                  [ta, sa, da, ta, sa, da, ta],
                  [narrow, narrow, narrow, narrow],%, narrow, narrow, narrow],
                  [start, non_start, start, non_start],%, start, non_start, start],
                  [2, 1, 2, 1]%, 2, 1, 2]
                  ]).

test_harm(Test) :- test_harm_example(Test, [N1, N2, N3, N4, Types, Widths, Measures, Strengths]),
                   %% parq([N1, N2, N3, N4]).
                   harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).

test(test31, [a, c, [p(a,d), p(b,e), p(b,f)], [c, d], [p(b,e), p(b,f)]], group1/5, positive).

run_test(T) :-
    test(T, [Top, Bass, Rest, B1, R1], group1/5, _),
    findall(x(A1, A2), group1(Top, Bass, Rest, A1, A2), [x(X, Y)]),
    same_elements(X, B1),
    same_elements(Y, R1).

matched_elements(_, [], []).
matched_elements(Pred, [X | XS], Y) :- append([A, [X1], B], Y),
                                       call(Pred, X, X1),
                                       append(A, B, Z),
                                       matched_elements(Pred, XS, Z), !.

% p - это пара. Сначала идут возможные гармонизации: p(a, c): a - 3 верхних
% голоса, c - бас.
% g - это группа. Потом тройки верхних голосов группируются, и к ним добавляются всевозможные басы.
test(test32, [p(a, c), p(a,d), p(b,e), p(b,f)], [g(a, [c, d]), g(b, [e, f])], groupHarms/2, positive).

matchg(g(X, A), g(X, B)) :- same_elements(A, B).
% Сначала берём из теста список пар (верхние голоса, бас) и идеальную группировку в B
%
runT(T) :- test(T, X, Ideal, groupHarms/2, positive),
           findall(G, groupHarms(X, G), [G]),
           matched_elements(matchg, G, Ideal).

runTestGroupHarm(T) :-
    test(T, [Top, Bass, Rest, B1, R1], group1/5, _),
    findall(x(A1, A2), group1(Top, Bass, Rest, A1, A2), [x(X, Y)]),
    same_elements(X, B1),
    same_elements(Y, R1).

% Тестовые данные для find_supremum
% 2 баса должны быть сведены
% 1 бас несводим к предыдущим двум
test(test33, [note(5, 1), note(5, 3)], [note(4, 1), note(4, 1)], [note(3, 1), note(3, 1)], isBetter/3, positive).
test(test34, [note(5, 1), note(5, 3)], [note(3, 1), note(3, 1)], [note(4, 1), note(4, 1)], isBetter/3, negative).
% первая лучше у первого, а вторая лучше у второго
test(test35, [note(5, 1), note(5, 3)], [note(4, 1), note(3, 1)], [note(3, 1), note(5, 1)], isBetter/3, negative).

runTest_isBetter(T) :- test(T, Tenor, Bass1, Bass2, isBetter/3, positive),
                      isBetter(Tenor, Bass1, Bass2).

runTest_isBetter(T) :- test(T, Tenor, Bass1, Bass2, isBetter/3, negative),
                      \+ isBetter(Tenor, Bass1, Bass2).


test(test34, [note(5, 1), note(5, 3)], [[note(4, 1), note(4, 1)], [note(3, 1), note(3, 1)], [note(4, 4), note(4, 1)]], find_supremum/3, positive).

runTestfind_supremum(T) :- test(T, Tenor, Bases, find_supremum/3, positive),
                           find_supremum(isBetter(Tenor), Bases, BestBases),
                           write(BestBases).

%% test_harm(Test) :- test_harm_neg_example(Test, [N1, N2, N3, N4, Types, Widths, Measures, Strengths]),
%%                    \+ harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).
%% test2(G), length(G, L), member(g(_, F), G), length(F, FL).
test_group_harm(Groups) :- test_harm_example(
                               melody4, [N1, _, _, _, _, _, Measures, Strengths]),
                           group_harm(N1, _, _, _, _, _, Strengths, Measures, Groups).

are_identical(X, Y) :-
    X == Y.

filterList(A, In, Out) :-
    exclude(are_identical(A), In, Out).

% С помощью предиката Comparator проверяет, что Comparator не выполняется для
is_in_supremum(_, []).
is_in_supremum(Comparator, [X | XS]) :- \+ apply(Comparator, [X]), is_in_supremum(Comparator, XS).

run_test_find_supremum() :- findall(Out,
                                    find_supremum(append([1]), [[1,2], [2], [1,5], [3]], Out),
                                    AllOuts),
                            same_selements(AllOuts, [[1,2], [1,5], [3]]).

% A-min
test_harm_example(melodyMin,
                  [
                  [note(5, 5)],
                  [start],
                  [2]
                  ]).

test_harm_example(melodyMin2,
                  [
                  [note(5, 5), note(5, 6)],
                  [start, non_start],
                  [2, 1]
                  ]).


test_harm_min(N1, Types, N2, N3, N4, Widths, Strengths, Measures) :- test_harm_example(
                                                                         melodyMin, [N1, Measures, Strengths]),
                                                                     harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).

test_group_harm_min(Groups) :- test_harm_example(
                                   melodyMin, [N1, Measures, Strengths]),
                               group_harm(N1, N2, N3, N4, Types,Widths, Strengths, Measures, Groups).

test_harm_min2(N1, Types, N2, N3, N4, Widths, Strengths, Measures) :- test_harm_example(
                                                                         melodyMin2, [N1, Measures, Strengths]),
                                                                     harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).


test_group_harm_min(Groups) :- test_harm_example(
                                   melodyMin, [N1, Measures, Strengths]),
                               group_harm(N1, N2, N3, N4, Types,Widths, Strengths, Measures, Groups).


test_group_harm_min2(Groups) :- test_harm_example(
                                    melodyMin2, [N1, Measures, Strengths]),
                                group_harm(N1, N2, N3, N4, Types,Widths, Strengths, Measures, Groups).

test_dup(X) :- findall(p([N1, N2, N3, Types, Widths], N4),
                           harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures),
                           Harms),
               append([_, [X], Y], Harms), member(X, Y).
