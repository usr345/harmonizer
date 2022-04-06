:- [harm].
:- [read2].
:- [write2].

%                 Scale = min or maj
harmonize(InFile, Scale, OutFile) :-
	readFileAndConvert(InFile, Scale, N1, N2, N3, N4, S, M, C, A), 
	harm(N1, TDS, N2, N3, N4, W, S, M), 
	writeChordsToFile(OutFile, C, A), !.
