%-*- mode: prolog-*-
:- use_module(pitch_arithm).
:- use_module(harm).
:- discontiguous(run_test/1).
:- discontiguous(test/4).
:- discontiguous(test_harm/1).

test(test1, [note(1, 5), note(3, 5)], stage_less/2, positive).
test(test2, [note(1, 5), note(1, 6)], stage_less/2, positive).

run_test(Test) :- test(Test, [N1, N2], stage_less/2, positive),
                       stage_less(N1, N2).

test(test3, [note(2, 5), note(1, 5)], stage_less/2, negative).
test(test4, [note(1, 5), note(1, 5)], stage_less/2, negative).

run_test(Test) :- test(Test, [N1, N2], stage_less/2, negative),
                  \+ stage_less(N1, N2).


test(test5, [note(1, 5), note(3, 5)], stage_le/2, positive).
test(test6, [note(1, 5), note(1, 6)], stage_le/2, positive).
test(test7, [note(1, 6), note(1, 6)], stage_le/2, positive).

run_test(Test) :- test(Test, [N1, N2], stage_le/2, positive),
                  stage_le(N1, N2).

test(test8, [note(3, 5), note(1, 2)], stage_le/2, negative).

run_test(Test) :- test(Test, [N1, N2], stage_le/2, negative),
                  \+ stage_less(N1, N2).

% notes_cmp
test(test9, [note(1, 1), note(1, 3), -1], notes_cmp, positive).
test(test10, [note(2, 3), note(2, 3), 0], notes_cmp, positive).
test(test11, [note(2, 5), note(2, 3), 1], notes_cmp, positive).

run_test(Test) :- test(Test, [N1, N2, Val], notes_cmp, positive),
                  findall(X, notes_cmp(N1, N2, X), [Val]).

% less_then_oct
test(test12, [note(5, 3), note(4, 3)], less_then_oct/2, positive).
test(test13, [note(2, 3), note(2, 3)], less_then_oct/2, positive).
test(test14, [note(5, 3), note(5, 2)], less_then_oct/2, positive).

run_test(Test) :- test(Test, [N1, N2], less_then_oct/2, positive),
                  less_then_oct(N1, N2).

test(test15, [note(5, 3), note(4, 2)], less_then_oct/2, negative).
test(test16, [note(5, 3), note(2, 2)], less_then_oct/2, negative).

run_test(Test) :- test(Test, [N1, N2], less_then_oct/2, negative),
                  \+ less_then_oct(N1, N2).


% задается нота 1, и по ней ищется нота 2, которая лежит ниже
test(test17, [note(2, 1), note(1, 5)], nearest_down/2, positive).
test(test18, [note(2, 3), note(1, 3)], nearest_down/2, positive).
test(test19, [note(2, 5), note(2, 3)], nearest_down/2, positive).
run_test(Test) :- test(Test, [N1, N2], nearest_down/2, positive),
                  findall(X, nearest_down(N1, X), [N2]).


% нота, [список разрешенных октав], ступень
test(test20, [note(5, 3), [5, 4], 1], nearest_down_bass/2, positive).
test(test21, [note(5, 3), [5, 4, 3], 3], nearest_down_bass/2, positive).

same_elements([], []).
same_elements([X | XS], Y) :- append([A, [X], B], Y),
                              append(A, B, Z),
                              same_elements(XS, Z), !.

%% same_elements([a, b, c, a], [c, a, b, a]).
%% same_elements([a, b, c, a], [c, a, b]).
%% same_elements([a, b, c, a], [c, a, a, b]).

run_test(Test) :- test(Test, [N, Octs, Stage], nearest_down_bass/2, positive),
                  findall(X, nearest_down_bass(N, note(X, Stage)), R),
                  same_elements(R, Octs).

test(test22, [note(3, 2), note(5, 3), -1], notes_cmp/3, positive).
test(test23, [note(3, 2), note(3, 2), 0], notes_cmp/3, positive).
test(test24, [note(3, 5), note(3, 2), 1], notes_cmp/3, positive).

run_test(Test) :- test(Test, [N1, N2, Val], notes_cmp/3, positive),
                  findall(X, notes_cmp(N1, N2, X), [Val]).

test(test25, [7, [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)], 12, note(0, 4)], altitude2note/4, positive).
test(test26, [7, [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)], 6, note(-1, 7)], altitude2note/4, positive).

run_test(Test) :- test(Test, [S, T, A, Val], altitude2note/4, positive),
                  findall(X, altitude2note(S, T, A, X), [Val]).

% Предикаты, на которые есть тесты
predicates_tested(Module, Set) :- findall(P, (module_property(Module, exports(X)), member(P, X), test(_, _, P, _)), List),
                                  list_to_set(List, Set).

predicates_not_tested(Module, Set) :- findall(P, (module_property(Module, exports(X)), member(P, X), \+ test(_, _, P, _)), List),
                                  list_to_set(List, Set).

test(test27, [[ta, sa, da, da, ta, da, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, positive).
test(test28, [[ta, ta, ta, ta, ta, ta, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, positive).

run_test(Test) :- test(Test, [T, Strengths], check_downbeat/2, positive),
                  check_downbeat(T, Strengths).

test(test27, [[ta, sa, da, da, ta, da, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, positive).
test(test28, [[ta, ta, ta, ta, ta, ta, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, positive).

run_test(Test) :- test(Test, [T, Strengths], check_downbeat/2, positive),
                  check_downbeat(T, Strengths).

test(test29, [[ta, sa, sa, da, ta, da, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, negative).
test(test30, [[ta, sa, da, da, da, ta, ta, ta], [4, 2, 3, 1, 4, 2, 3, 1]], check_downbeat/2, negative).

run_test(Test) :- test(Test, [T, Strengths], check_downbeat/2, negative),
                  \+ check_downbeat(T, Strengths).

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
                  [note(5, 5), note(5, 6), note(5, 5), note(5, 3), note(5, 4), note(5, 2), note(5, 1)],
                  [note(5, 3), note(5, 4), note(5, 2), note(5, 1), note(5, 1), note(4, 7), note(4, 5)],
                  [note(5, 1), note(5, 1), note(4, 7), note(4, 5), note(4, 6), note(4, 5), note(4, 3)],
                  [note(4, 1), note(3, 4), note(3, 5), note(4, 1), note(3, 4), note(3, 5), note(4, 1)],
                  [ta, sa, da, ta, sa, da, ta],
                  [narrow, narrow, narrow, narrow, narrow, narrow, narrow],
                  [start, non_start, start, non_start, start, non_start, start],
                  [2, 1, 2, 1, 2, 1, 2]
                  ]).

test_harm(Test) :- test_harm_example(Test, [N1, N2, N3, N4, Types, Widths, Measures, Strengths]),
                   %% parq([N1, N2, N3, N4]).
                   harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).

run_test(Test) :- test(Test, [S, T, A, Val], altitude2note/4, positive),
                  findall(X, altitude2note(S, T, A, X), [Val]).

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



%% test_harm(Test) :- test_harm_neg_example(Test, [N1, N2, N3, N4, Types, Widths, Measures, Strengths]),
%%                    \+ harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).
%% test2(G), length(G, L), member(g(_, F), G), length(F, FL).
test2(Groups) :- test_harm_example(
                     melody4, [N1, _, _, _, _, _, Measures, Strengths]),
                 findall(p([N1, N2, N3, Types, Widths], N4),
                         harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures),
                         Harms),
                 groupHarms(Harms, Groups).

% выбирает список базов [B1 | B1S], которые не хуже при гармонизации, чем [B2 | B2S]
% [T | TS] - список теноров
% [B1 | B1S] - первый список басов, который isBetter
% [B2 | B2S] - второй список басов, который не isBetter
% Если первый nearest_down, то второй нам не важен
isBetter([T | TS], [_ | B2S], [B1 | B1S]) :- nearest_down(T, B1), isBetter(TS, B1S, B2S), !.
% Если второй не nearest_down, то первый нам не важен
isBetter([T | TS], [B2 | B2S], [_ | B1S]) :- \+ nearest_down(T, B2), isBetter(TS, B1S, B2S).

are_identical(X, Y) :-
    X == Y.

filterList(A, In, Out) :-
    exclude(are_identical(A), In, Out).

is_in_supremum(_, []).
is_in_supremum(Comparator, [X | XS]) :- \+ apply(Comparator, [X]), is_in_supremum(Comparator, XS).

% В Comparator содержится isBetter([Tenor | TS])
find_supremum(Comparator, List, OutBestBass) :-
    % List - это список басов
    % Перебираем все возможные басы из списка в поисках наилучшего
    append([A, [OutBestBass], Z], List),
    % Перебирает все возможные пары голосов
    % В T = [isBetter, [Tenor | TS]]
    Comparator =.. T,
    % В T2 := [isBetter, [Tenor | TS], OutBestBass]
    append(T, [OutBestBass], T2),
    % C1 := isBetter([Tenor | TS], OutBestBass)
    C1 =.. T2,
    is_in_supremum(C1, A),
    is_in_supremum(C1, Z).

run_test_find_supremum() :- findall(Out,
                                       find_supremum(append([1]), [[1,2], [2], [1,5], [3]], Out),
                                       AllOuts),
                               same_elements(AllOuts, [[1,2], [1,5], [3]]).

%% test(test33, , group1/5, positive).
