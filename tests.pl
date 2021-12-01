%-*- mode: prolog-*-
:- use_module(pitch_arithm).
:- use_module(harm).

test(notes1, [note(1, 5), note(3, 5)], stage_less, positive).
test(notes2, [note(1, 5), note(1, 6)], stage_less, positive).

test_positive(Test) :- test(Test, [N1, N2], stage_less, positive),
                       stage_less(N1, N2).

test(notes1, [note(2, 5), note(1, 5)], stage_less, negative).
test(notes2, [note(1, 5), note(1, 5)], stage_less, negative).

test_negative(Test) :- test(Test, [N1, N2], stage_less, negative),
                       \+ stage_less(N1, N2).


test(notes1, [note(1, 5), note(3, 5)], stage_le, positive).
test(notes2, [note(1, 5), note(1, 6)], stage_le, positive).
test(notes3, [note(1, 6), note(1, 6)], stage_le, positive).

test_positive_le(Test) :- test(Test, [N1, N2], stage_le, positive),
                          stage_le(N1, N2).

test2(notes1, [note(3, 5), note(1, 2)], stage_le, negative).

test_negative_le(Test) :- test(Test, [N1, N2], stage_le, negative),
                          \+ stage_less(N1, N2).

% notes_cmp
test(notes1, [note(1, 1), note(1, 3), -1], notes_cmp, positive).
test(notes2, [note(2, 3), note(2, 3), 0], notes_cmp, positive).
test(notes3, [note(2, 5), note(2, 3), 1], notes_cmp, positive).

test_positive_notes_cmp(Test) :- test(Test, [N1, N2, Val], notes_cmp, positive),
                                 findall(X, notes_cmp(N1, N2, X), [Val]).

% less_then_oct
test(notes1, [note(5, 3), note(4, 3)], less_then_oct, positive).
test(notes2, [note(2, 3), note(2, 3)], less_then_oct, positive).
test(notes3, [note(5, 3), note(5, 2)], less_then_oct, positive).

test_positive_less_then_oct(Test) :- test(Test, [N1, N2], less_then_oct, positive),
                                     less_then_oct(N1, N2).

test(notes1, [note(5, 3), note(4, 2)], less_then_oct, negative).
test(notes2, [note(5, 3), note(2, 2)], less_then_oct, negative).

test_negative_less_then_oct(Test) :- test(Test, [N1, N2], less_then_oct, negative),
                                     \+ less_then_oct(N1, N2).


% задается нота 1, и по ней ищется нота 2, которая лежит ниже
test(notes1, [note(2, 1), note(1, 5)], nearest_down, positive).
test(notes2, [note(2, 3), note(1, 3)], nearest_down, positive).
test(notes3, [note(2, 5), note(2, 3)], nearest_down, positive).
test_nearest_down_positive(Test) :- test(Test, [N1, N2]),
                                    findall(X, nearest_down(N1, X), [N2]).


% нота, [список разрешенных октав], ступень
test(notes1, [note(5, 3), [5, 4], 1], nearest_down_bass, positive).
test(notes2, [note(5, 3), [5, 4, 3], 3], nearest_down_bass, positive).

same_elements([], []).
same_elements([X | XS], Y) :- append([A, [X], B], Y), append(A, B, Z),
                              same_elements(XS, Z), !.

%% same_elements([a, b, c, a], [c, a, b, a]).
%% same_elements([a, b, c, a], [c, a, b]).
%% same_elements([a, b, c, a], [c, a, a, b]).

test_nearest_down_bass_positive(Test) :- test(Test, [N, Octs, Stage], nearest_down_bass, positive),
                                         findall(X, nearest_down_bass(N, note(X, Stage)), R),
                                         same_elements(R, Octs).

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
