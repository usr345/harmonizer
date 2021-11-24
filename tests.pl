%-*- mode: prolog-*-
:- use_module(pitch_arithm).

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

%% stages_eq
