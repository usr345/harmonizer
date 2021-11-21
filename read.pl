:- module(read, [readNotes/5]).

:- use_module(utility).
:- use_module(pitch_arithm).
:- use_module(library(musicxml)).

markMeasure(_, [], []).
markMeasure(V, [_|M], [V|R]) :- markMeasure(non_start, M, R).
markMeasure(M, R) :- markMeasure(start, M, R).

getAlter(MNote, Alter) :-
       member(element(alter, _, [AlterChar]), MNote), atom_number(AlterChar, Alter), !.
getAlter(_, 0).

getNote(MNote, xnote(Octave, StepChar, Alter)) :-
       member(element(step, _, [StepChar]), MNote),
       member(element(octave, _, [OctaveChar]), MNote),
       atom_number(OctaveChar, Octave),
       getAlter(MNote, Alter).

getNotes([], []).
getNotes([element(note, _, [element(pitch, _, MNote) | _])|Tail], [XNote|OTail]) :-
       getNote(MNote, XNote),
       getNotes(Tail, OTail), !.
getNotes([_|Tail], OTail) :- getNotes(Tail, OTail).

getNotesFromParts(P, XNotes, Measures) :-
       filterElements(measure, P, M),
       maplist(getNotes, M, XNotesLists),
       append(XNotesLists, XNotes),

       %write(XNotesLists),
       %maplist(force, XNotesLists, ForcesLists),
       %append(ForcesLists, Forces),

       maplist(markMeasure, XNotesLists, Marks),
       append(Marks, Measures).

/*
Фунция считывает массив нот из файла
- File - имя файла с расширением xml или mxml
- XNotes - массив нот в представлении xnote(4, 'C', 0) - xnote(октава, нота в виде атома, альтерация: -1, 0, 1)
- Alts - абсолютная величина нот
- Marks - массив маркеров начала тактов [start, non_start, non_start, start, non_start, non_start]
*/
readMXML(File, XNotes, Alts, Marks, Key) :-
       musicxml_score(File, element(_, _, S)),
       getElements(part, S, P),
       getElements(measure, P, M),
       getElements(attributes, M, A),
       getElements(key, A, Key),
%       getNotes(M, XNotes),
       getNotesFromParts(P, XNotes, Marks),
       altitudes(XNotes, Alts).

/*
- File (вх) - имя файла
- Shift (вх) - абсолютная величина тоники - от 0 до 11
- Scale (вх) - maj, min
- Notes (исх) -
- Marks (исх) -
*/
readNotes(File, Shift, Scale, Notes, Marks) :-
       readMXML(File, _, Alts, Marks),
       tons(Scale, List),
       altitudes2notes(Shift, List, Alts, Notes).
