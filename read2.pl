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
       appendNote(N, ts(MeasureNumber,NTime), Voice, NDur, MT, Notes),!.

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



checkCorrectAlter(_, 0, 0).
checkCorrectAlter(_, 1, 0).
checkCorrectAlter(_, 2, 0).
checkCorrectAlter(_, 3, 0).
checkCorrectAlter(_, 4, 0).
checkCorrectAlter(_, 5, 0).
checkCorrectAlter(maj, 6, 0).
checkCorrectAlter(min, 6, 1).

checkCorrectAlters(Scale, [chord([stage(_, S1, A1), stage(_, S2, A2), stage(_, S3, A3), stage(_, S4, A4)], _, _, _) | Tail ]) :-
   checkCorrectAlter(Scale, S1, A1),
   checkCorrectAlter(Scale, S2, A2),
   checkCorrectAlter(Scale, S3, A3),
   checkCorrectAlter(Scale, S4, A4),
   checkCorrectAlters(Scale, Tail).

checkCorrectAlters(_, []).

convertMusicFormat([(O1, S1) | Tail1], [(O2, S2) | Tail2], [(O3, S3) | Tail3], [(O4, S4) | Tail4],
                   [chord([stage(O1, S1, A1), stage(O2, S2, A2), stage(O3, S3, A3), stage(O4, S4, A4)], _, _, _) | Tail ]) :-
    convertMusicFormat(Tail1, Tail2, Tail3, Tail4, Tail).

convertMusicFormat([], [], [], [], []).

% конвертировать разные форматы друг в друга, проверяя корректность
% альтераций
convertAndCheck(Scale, N1, N2, N3, N4, Chords) :-
   convertMusicFormat(N1, N2, N3, N4, Chords),
   checkCorrectAlters(Scale, Chords).
