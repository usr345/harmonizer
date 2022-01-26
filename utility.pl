%-*- mode: prolog-*-
:- module(utility, [getElements/3, filterElements/3, rnext/3, find_supremum/3]).

getElements(_, [], []).
getElements(E, [element(E, _, M) | T], MO) :- getElements(E, T, MT), append(M, MT, MO), !.
getElements(E, [_|T], MO) :- getElements(E, T, MO).

filterElements(_, [], []).
filterElements(E, [element(E, _, M) | T], [M | MT]) :- filterElements(E, T, MT), !.
filterElements(E, [_|T], MO) :- filterElements(E, T, MO).

% данное отношение является вспомогательным для rnext
% Поиск следующего элемента в зацикленном списке
% параметры отношения:
% (элемент, [список], следующий элемент, первый элемент изначального списка)
xnext(N, [N, A | _], A, _).
xnext(N, [_ , X | M], A, F) :- xnext(N, [X | M], A, F).
% Последний элемент соседний с первым
xnext(N, [N], A, A).

% Следующий элемент в циклическом списке
rnext(N, [H | T], A) :- xnext(N, [H | T], A, H).

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
