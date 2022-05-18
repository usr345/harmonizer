%-*- mode: prolog-*-
%% findall(P, (absolute_file_name('tests.pl', X), source_file(P, X)), L).
:- use_module(library(st/st_render)).
:- use_module(library(clpfd)).
:- use_module(library(readutil)).
:- use_module(tests).

getDef([], []).
getDef([:- _ | I], O) :- getDef(I, O), !.
getDef([D :- _ | I], [D | O]) :- getDef(I, O), !.
getDef([D | I], [D | O]) :- getDef(I, O).

sig(T, N/P) :- T =.. [N | A], length(A, P).

sc([A], _, A).
sc([A | B], S, O) :- sc(B, S, O1), string_concat(A, S, P), string_concat(P, O1, O).

by_type(Name, Type, Len) :-
    findall(N, test(N, _, Name, Type), List), length(List, Len).

escape_str(Input, Output) :-
    split_string(Input, "_", "", L),
    sc(L, "\\_", Output).

% Делаем имена протестированных предикатов жирными
wrapbf(Name, Name, 0) :- !.
wrapbf(Name, NameWrapped, _) :-
    string_concat("\\textbf{", Name, Temp),
    string_concat(Temp, "}", NameWrapped).

converter(Types, X, _{ name: Name, tests: List, total: SumList}) :-
        term_string(X, X1),
        maplist(by_type(X), Types, List),
        foldl(plus, List, 0, SumList),
        escape_str(X1, X2),
        wrapbf(X2, Name, SumList).

%
readpl(File, Terms) :-
    read_file_to_terms(File, L, []),
    getDef(L, D),
    maplist(sig, D, Terms).

write_to_file(FileName, Tests, FilesInfo) :-
    open(FileName, write, FH),
    st_render_file('output/output', _{
        headers: Tests,
        files: FilesInfo
    }, FH, _{
        frontend: syntax_tokens(
            comment("<%#", "%>"),
            out("<%=", "%>"),
            out_unescaped("<%~", "%>"),
            statement("<%", "%>")
        )
           }),
    close(FH).

process_file(Tests, File, _{ name: FileName, items: List}) :-
    readpl(File, Terms),
    get_unique_test_types(Tests),
    list_to_set(Terms, Terms1),
    maplist(converter(Tests), Terms1, List),
    escape_str(File, FileName).

do_work(FilesList, Content) :-
    get_unique_test_types(Tests),
    maplist(process_file(Tests), FilesList, Content),
    write_to_file("output/output.tex", Tests, Content).


get_unique_test_types(Set) :- findall(T, test(_, _, _, T), List),
                              list_to_set(List, Set).

% Предикаты, на которые есть тесты
predicates_tested(Module, Set) :- findall(P, (module_property(Module, exports(X)), member(P, X), test(_, _, P, _)), List),
                                  list_to_set(List, Set).

predicates_not_tested(Module, Set) :- findall(P, (module_property(Module, exports(X)), member(P, X), \+ test(_, _, P, _)), List),
                                  list_to_set(List, Set).
