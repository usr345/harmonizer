%-*- mode: prolog-*-
:- use_module(library(clpfd)).
:- use_module(steps).

get_same_elements([], _, []).

get_same_elements([X | XS], Second, [X | Out]) :-
    member(X, Second),
    get_same_elements(XS, Second, Out).

get_same_elements([X | XS], Second, Out) :-
    \+ member(X, Second),
    get_same_elements(XS, Second, Out).

% получает 2 тональности, и на выходе - список общих нот
compare_scales(Note1, Scale1, Note2, Scale2, Out) :-
    build_scale_human(Note1, Scale1, Notes1),
    build_scale_human(Note2, Scale2, Notes2),
    get_same_elements(Notes1, Notes2, Out).

%% Найти все тональности, имеющие с C-maj 2 общие ноты
%% member(Alt, [0, 1, -1]), compare_scales(note('C', 0), maj, note(X, Alt), T1, [N1, N2]).

% TODO: фильтрация тональностей с приведением к каноническому виду
