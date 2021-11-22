checkIndividualChord(Key, Chord).

checkIndividualChords(Key, [Chord| Rest]) :-
   checkIndividualChord(Key, Chord),
   checkIndividualChords(Key, Rest).

checkIndividualChords(_, []).

% TODO: check voice pair
checkVoicePairChord(V1, V2, Chord).

checkVoicePairChords(V1, V2, [Chord| Rest]) :-
   checkVoicePairChord(V1, V2, Chord),
   checkVoicePairChords(V1, V2, Rest).

checkVoicePairChords(_, _, []).

% TODO: check chord pair
checkChordPair(Attrs, Chord1, Chord2).

checkChordPairs(Attrs, [Chord1, Chord2 | Rest]) :-
   checkChordPair(Attrs, Chord1, Chord2),
   checkChordPairs(Attrs, [Chord2 | Rest] ).

checkChordPairs(_, [_]).

harm2(music_attrs(Key, Beats), Chords) :-
   checkIndividualChords(Key, Chords),
   checkChordPairs(music_attrs(Key, Beats), Chords),
   checkVoicePairChords(0, 1, Chords),
   checkVoicePairChords(0, 2, Chords),
   checkVoicePairChords(0, 3, Chords),
   checkVoicePairChords(1, 2, Chords),
   checkVoicePairChords(1, 3, Chords),
   checkVoicePairChords(2, 3, Chords).

