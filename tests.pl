%-*- mode: prolog-*-
:- use_module(pitch_arithm).
:- use_module(harm).

test(notes1, [note(1, 5), note(3, 5)]).
test(notes2, [note(1, 5), note(1, 6)]).

test_positive(Test) :- test(Test, [N1, N2]),
                       stage_less(N1, N2).

test_neg(notes1, [note(2, 5), note(1, 5)]).
test_neg(notes2, [note(1, 5), note(1, 5)]).

test_negative(Test) :- test_neg(Test, [N1, N2]),
                       \+ stage_less(N1, N2).


test2(notes1, [note(1, 5), note(3, 5)]).
test2(notes2, [note(1, 5), note(1, 6)]).
test2(notes3, [note(1, 6), note(1, 6)]).

test_positive_le(Test) :- test2(Test, [N1, N2]),
                          stage_le(N1, N2).

test_positive_le(Test) :- test2(Test, [N1, N2]),
                          stage_le(N1, N2).

test2_neg(notes1, [note(3, 5), note(1, 2)]).

test_negative_le(Test) :- test2_neg(Test, [N1, N2]),
                          \+ stage_less(N1, N2).

% задается нота 1, и по ней ищется нота 2, которая лежит ниже
test3(notes1, [note(2, 1), note(1, 5)]).
test3(notes2, [note(2, 3), note(1, 3)]).
test3(notes3, [note(2, 5), note(2, 3)]).
test_nearest_down_positive(Test) :- test3(Test, [N1, N2]),
                                    nearest_down(N1, N2).


test3_neg(notes2, [note(2, 3), note(2, 5)]).
test3_neg(notes2, [note(2, 3), note(2, 3)]).
test_nearest_down_negative(Test) :- test3_neg(Test, [N1, N2]),
                                    \+ nearest_down(N1, N2).

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
