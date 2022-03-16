:- [utility].

:- use_module(pitch_arithm2).
:- use_module(library(musicxml)).
:- use_module(library(clpfd)).

getAlter2(MNote, Alter) :-
       member(element(alter, _, [AlterChar]), MNote), atom_number(AlterChar, Alter), !.
getAlter2(_, 0).

getVoice('1', '1', 1).
getVoice('2', '1', 2).
getVoice('5', '2', 1).
getVoice('6', '2', 2).

getNoteAttrs(E, Voice, Duration) :-
       member(element(duration, _, [D]), E),
       member(element(voice, _, [V]), E),
       member(element(staff, _, [S]), E),
       getVoice(V, S, Voice),
       atom_number(D, Duration).

readPitch(P, Octave, StepChar, Alter) :-
       member(element(step, _, [StepChar]), P),
       member(element(octave, _, [OctaveChar]), P),
       atom_number(OctaveChar, Octave),
       getAlter2(P, Alter).

appendNote(Elem, Ts, Voice, Duration, Tail, DstList) :-
       member(element(pitch, _, P), Elem),
       readPitch(P, Octave, Step, Alter),
       append([xml_note(Ts, voice(Voice), duration(Duration), pitch(Octave, Step, Alter))], Tail, DstList ).

appendNote(_, _, _, _, Tail, DstList) :-
       DstList = Tail.

getNotesFromMeasureInternal([], _, _, [], _).

getNotesFromMeasureInternal([element(note, _, N) | Tail], MeasureNumber, Mult, Notes, Time) :-
       getNoteAttrs(N, Voice, Duration),
       getNotesFromMeasureInternal(Tail, MeasureNumber, Mult, MT, Time + Duration * Mult),
       NTime #= Time,
       NDur #= Duration * Mult,
       appendNote(N, ts(MeasureNumber, NTime), Voice, NDur, MT, Notes),!.

getNotesFromMeasureInternal([element(backup, _, B) | Tail], MeasureNumber, Mult, Notes, Time) :-
       member(element(duration, _, [Dur]), B),
       atom_number(Dur, Backup),
       getNotesFromMeasureInternal(Tail, MeasureNumber, Mult, Notes, Time - Backup * Mult), !.

getNotesFromMeasureInternal([_ | Tail], MeasureNumber, Mult, Notes, Time) :-
       getNotesFromMeasureInternal(Tail, MeasureNumber, Mult, Notes, Time).

getNotesFromMeasure(M, MeasureNumber, DurationMult, Notes) :-
       getNotesFromMeasureInternal(M, MeasureNumber, DurationMult, Notes, 0).

checkFifth(M, Fifth) :-
       member(element(attributes, _, A), M),
       member(element(key, _, K), A),
       member(element(fifths, _, [F]), K),
       atom_number(F, Fifth), !.

checkFifth(_, _).

checkBeats(M, Beats) :-
       member(element(attributes, _, A), M),
       member(element(time, _, T), A),
       member(element(beats, _, [B]), T),
       member(element('beat-type', _, [BT]), T),
       atom_number(B, RawBeats),
       atom_number(BT, BeatType),
       parseBeats(RawBeats, BeatType, Beats), !.

checkBeats(_,_).

checkDurationMult(M, DurationMult) :-
       member(element(attributes, _, Attrs), M),
       member(element(divisions, _, [D]), Attrs),
       atom_number(D, Divisions),
       DurationMult #= 720 div Divisions.

checkDurationMult(_,_).

getNotesFromMeasures([element(measure, _,M) | Tail], StartNumber, Notes, Fifth, Beats, DurationMult) :-
       checkFifth(M, Fifth),
       checkBeats(M, Beats),
       checkDurationMult(M, DurationMult),

       NextMeasureNum #= StartNumber + 1,
       getNotesFromMeasure(M, StartNumber, DurationMult, C),
       getNotesFromMeasures(Tail, NextMeasureNum, NS, Fifth, Beats, DurationMult),

       append([C, NS], Notes), !.

getNotesFromMeasures([_ | Tail], StartNumber, Notes, Fifth, Beats, DurationMult) :-
       getNotesFromMeasures(Tail, StartNumber, Notes, Fifth, Beats, DurationMult).

getNotesFromMeasures([],_,[], _, _, _).

applyScale(maj, Stage, Alter, Key) :-
       Key = key(Stage, Alter, maj), !.

applyScale(min, Stage, Alter, Key) :-
       add_interval(pitch(0, Stage, Alter), interval(-2, -3), pitch(_, NewStage, NewAlter)),
       Key = key(NewStage, NewAlter, min), !.

parseKey(Scale, Fifth, Key) :-
       Stage #= (Fifth * 4) mod 7,
       Octave #= (Fifth * 4) div 7,
       abs_pitch(pitch(Octave, Stage, Alter), Fifth * 7),
       applyScale(Scale, Stage, Alter, Key), !.

parseBeats(RawBeats, BeatType, Beats) :-
       BeatLength #= (720 * 4) div BeatType,
       Beats = beats(RawBeats, BeatLength).

convOctave(Stage1, Stage2, Octave1, Octave2) :-
       Stage1 #< Stage2, Octave2 #= Octave1 - 1, !.

convOctave(_, _, Octave1, Octave2) :-
       Octave2 #= Octave1.

stageNum('C', 0).
stageNum('D', 1).
stageNum('E', 2).
stageNum('F', 3).
stageNum('G', 4).
stageNum('A', 5).
stageNum('B', 6).

scaleStageInterval(maj, Stage, Interval) :-
       nth0(Stage, [
                interval(0,0),
                interval(1,2),
                interval(2,4),
                interval(3,5),
                interval(4,7),
                interval(5,9),
                interval(6,11)], Interval), !.

scaleStageInterval(min, Stage, Interval) :-
       nth0(Stage, [
                interval(0,0),
                interval(1,2),
                interval(2,3),
                interval(3,5),
                interval(4,7),
                interval(5,8),
                interval(6,10)], Interval), !.

% Конвертируем альтерированную ступень Stage1, Alter1 в тональности key
% в ноту Stage2, Alter2
noteInKey(key(KeyStage, KeyAlter, KeyScale), Stage1, Alter1, Stage2, Alter2) :-
       scaleStageInterval(KeyScale, Stage1, Interval),
       add_interval(pitch(1, KeyStage, KeyAlter + Alter1), Interval, pitch(_, Stage2, Alter2)).

% Конвертируем сырой питч в ступень тональности с альтерацией и октавой
convertPitch(pitch(Octave1, Stage1, Alter1), key(KeyStage, KeyAlter, KeyScale), stage(Octave2, Stage2, Alter2)) :-
       stageNum(Stage1, Stage1Num),
       Stage2 #= (Stage1Num - KeyStage) mod 7,
       % находим октаву
       convOctave(Stage1Num, Stage2, Octave1, Octave2),
       % находим Alter2
       noteInKey(key(KeyStage, KeyAlter, KeyScale), Stage2, Alter2, Stage1Num, Alter1).

getMusicFromXML(File, Scale, Notes, music_attrs(Key, Beats)) :-
       musicxml_score(File,element(_, _, C)), getElements(part, C, P), getNotesFromMeasures(P, 0, Notes, Fifth, Beats, _), parseKey(Scale, Fifth, Key).


tsListRaw([xml_note(Ts, _, _, _) | Tail], TsList) :-
       tsListRaw(Tail, LR),
       append([Ts], LR, TsList), !.

tsListRaw([], []).

tsList(Notes, TsList) :-
       tsListRaw(Notes, TsListRaw),
       sort(TsListRaw, TsList), !.


forceVoices(Key, TsList, [xml_note(Ts, voice(Voice), Dur, Pitch) | NotesTail], Chords) :-
       nth0(Index, TsList, Ts),
       nth0(Index, Chords, chord(Stages, Ts, Dur, type(_))),
       Stages = [_,_,_,_],
       VoiceIdx #= Voice - 1,
       convertPitch(Pitch, Key, Stage),
       nth0(VoiceIdx, Stages, Stage),
       forceVoices(Key, TsList, NotesTail, Chords), !.

forceVoices(_, _, [], _).

getChords(Key, Notes, Chords) :-
       tsList(Notes, TsList),
       same_length(TsList, Chords),
       forceVoices(Key, TsList, Notes, Chords).

%           min or maj
%           |  harm1 note
%           |  |  harm2 note
%           |  |  |  harm2 note alteration
convertNote(_, 1, 0, 0).
convertNote(_, 2, 1, 0).
convertNote(_, 3, 2, 0).
convertNote(_, 4, 3, 0).
convertNote(_, 5, 4, 0).
convertNote(_, 6, 5, 0).
convertNote(maj, 7, 6, 0).
convertNote(min, 7, 6, 1).

convertMeasureTS(start, 0).
convertMeasureTS(non_start, X) :- X #> 0.

fraqEq(fraq(Nom1,Den1),fraq(Nom2,Den2)) :-
       Nom1 * Den2 #= Nom2 * Den1.

% начало такта имеет силу 4
convertStrengthTS(4, 0, _) :- !.
% размеры 2/4, 2/8...
convertStrengthTS(3, TS, beats(2, Den)) :- fraqEq(fraq(TS,Den), fraq(1,1)), !.
convertStrengthTS(2, TS, beats(2, Den)) :- fraqEq(fraq(TS,Den), fraq(1,2)), !.
convertStrengthTS(2, TS, beats(2, Den)) :- fraqEq(fraq(TS,Den), fraq(3,2)), !.
convertStrengthTS(1, _, beats(2, _)).
% размеры 3/4, 3/8...
convertStrengthTS(3, TS, beats(3, Den)) :- fraqEq(fraq(TS,Den), fraq(1,1)), !.
convertStrengthTS(3, TS, beats(3, Den)) :- fraqEq(fraq(TS,Den), fraq(2,1)), !.
convertStrengthTS(1, _, beats(3, _)).
% размеры 4/4, 4/8...
convertStrengthTS(3, TS, beats(4, Den)) :- fraqEq(fraq(TS,Den), fraq(2,1)), !.
convertStrengthTS(2, TS, beats(4, Den)) :- fraqEq(fraq(TS,Den), fraq(1,1)), !.
convertStrengthTS(2, TS, beats(4, Den)) :- fraqEq(fraq(TS,Den), fraq(3,1)), !.
convertStrengthTS(1, _, beats(4, _)).
% размеры 6/4, 6/8...
convertStrengthTS(3, TS, beats(6, Den)) :- fraqEq(fraq(TS,Den), fraq(3,1)), !.
convertStrengthTS(2, TS, beats(6, Den)) :- fraqEq(fraq(TS,Den), fraq(1,1)), !.
convertStrengthTS(2, TS, beats(6, Den)) :- fraqEq(fraq(TS,Den), fraq(2,1)), !.
convertStrengthTS(2, TS, beats(6, Den)) :- fraqEq(fraq(TS,Den), fraq(4,1)), !.
convertStrengthTS(2, TS, beats(6, Den)) :- fraqEq(fraq(TS,Den), fraq(5,1)), !.
convertStrengthTS(1, _, beats(6, _)).

% конвертировать между форматами harm1 и harm2
convertMusicFormat(Scale, Beats, [note(O1, SA1) | Tail1], [note(O2, SA2) | Tail2], [note(O3, SA3) | Tail3], [note(O4, SA4) | Tail4],
                     [Strength | Strengths], [Measure| Measures],
                   [chord([stage(O1, SB1, AB1), stage(O2, SB2, AB2), stage(O3, SB3, AB3), stage(O4, SB4, AB4)], ts(_, TS), _, _) | Tail ]) :-
    convertNote(Scale, SA1, SB1, AB1),
    convertNote(Scale, SA2, SB2, AB2),
    convertNote(Scale, SA3, SB3, AB3),
    convertNote(Scale, SA4, SB4, AB4),
    convertMeasureTS(Measure, TS),
    convertMeasureTS(Measure, TS),
    convertStrengthTS(Strength, TS, Beats),
    convertStrengthTS(Strength, TS, Beats),
    convertMusicFormat(Scale, Beats, Tail1, Tail2, Tail3, Tail4, Strengths, Measures, Tail).

convertMusicFormat(_, _, [], [], [], [], [], [], []).

readFileAndConvert(
       File, Scale,
       % read1 data:
       Notes1, Notes2, Notes3, Notes4, Strength, Measures, 
       % read2 data:
       Chords, music_attrs(Key, Beats)) :-
    getMusicFromXML(File, Scale, Notes, music_attrs(Key, Beats)),
    getChords(Key, Notes, Chords),
    convertMusicFormat(Scale, Beats, Notes1, Notes2, Notes3, Notes4, Strength, Measures, Chords).
