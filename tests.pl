%-*- mode: prolog-*-
:- use_module(pitch_arithm).
:- use_module(harm).
:- discontiguous(run_test/1).
:- discontiguous(test/4).

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
same_elements([X | XS], Y) :- append([A, [X], B], Y), append(A, B, Z),
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
%% nearest_down_bass

%% test_harm_example(melody1, [[note(5, 3), note(5, 1)], [note(4, 5), note(4, 3)], [note(4, 1), note(3, 5)], [note(3, 1), note(3, 1)], [ta, ta], [wide, wide], [start, non_start], [2, 1]]).

% wide / narrow
test_harm_example(melody2, [[note(5, 3), note(5, 1)], [note(4, 5), note(4, 5)], [note(4, 1), note(4, 3)], [note(3, 1), note(3, 1)], [ta, ta], [wide, narrow], [start, non_start], [2, 1]]).

%% test_harm_neg_example(melody3, [[note(5, 3), note(5, 1)], [note(4, 5), note(4, 3)], [note(4, 1), note(3, 5)], [note(3, 1), note(3, 1)], [ta, ta], [wide, narrow], [start, non_start], [2, 1]]).

test_harm(Test) :- test_harm_example(Test, [N1, N2, N3, N4, Types, Widths, Measures, Strengths]),
                   %% parq([N1, N2, N3, N4]).
                   harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).

%% test_harm(Test) :- test_harm_neg_example(Test, [N1, N2, N3, N4, Types, Widths, Measures, Strengths]),
%%                    \+ harm(N1, Types, N2, N3, N4, Widths, Strengths, Measures).

%% test_nearest_down_bass
%% stages_eq
