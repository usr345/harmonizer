%-*- mode: prolog-*-
%% findall(P, (absolute_file_name('tests.pl', X), source_file(P, X)), L).
:- use_module(library(st/st_render)).
:- use_module(library(clpfd)).
:- use_module(library(readutil)).

getDef([], []).
getDef([:- _ | I], O) :- getDef(I, O), !.
getDef([D :- _ | I], [D | O]) :- getDef(I, O), !.
getDef([D | I], [D | O]) :- getDef(I, O).

sig(T, N/P) :- T =.. [N | A], length(A, P).

sc([A], _, A).
sc([A | B], S, O) :- sc(B, S, O1), string_concat(A, S, P), string_concat(P, O1, O).

converter(X, _{ name: X2, tested: '0'}) :- term_string(X, X1), split_string(X1, "_", "", L), sc(L, "\\_", X2).

%
readpl(File, Terms) :- read_file_to_terms(File, L, []), getDef(L, D), maplist(sig, D, Terms).

write_to_file(FileName, Items) :-
    open(FileName, write, FH),
    st_render_file('output/output', _{
        items: Items
    }, FH, _{
        frontend: syntax_tokens(
            comment("<%#", "%>"),
            out("<%=", "%>"),
            out_unescaped("<%~", "%>"),
            statement("<%", "%>")
        )
           }),
    close(FH).

do_work(File, List) :-
    readpl(File, Terms),
    list_to_set(Terms, Terms1),
    maplist(converter, Terms1, List),
    write_to_file("output/output.tex", List).
