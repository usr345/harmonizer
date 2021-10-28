%-*- mode: prolog-*-
:- use_module(library(clpfd)).
:- use_module(library(musicxml)).

stage_less(note(Octave1, _), note(Octave2, _)) :- Octave1 #< Octave2.
stage_less(note(Octave, Stage1), note(Octave, Stage2)) :- Stage1 #< Stage2.

stage_le(Stage1, Stage1).
stage_le(Stage1, Stage2) :- stage_less(Stage1, Stage2).

stages_eq(note(Octave, Stage), note(Octave, Stage)).

stage_to_abs(note(Octave, Stage), X) :- X #= Octave*7 + Stage.

stages_ne(note(Octave1, _), note(Octave2, _)) :- Octave1 #\= Octave2.
stages_ne(note(Octave, Stage1), note(Octave, Stage2)) :- Stage1 #\= Stage2.

notes_cmp(X, Y, 0) :- stages_eq(X, Y).
notes_cmp(X, Y, -1) :- stage_less(X, Y).
notes_cmp(X, Y, 1) :- stage_less(Y, X).

less_then_oct(note(Octave, Stage), note(Octave, Stage)).
less_then_oct(note(Octave1, Stage1), note(Octave2, Stage2)) :- nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)).
less_then_oct(note(Octave1, Stage1), note(Octave2, Stage2)) :- nearest_down(note(Octave2, Stage2), note(Octave1, Stage1)).

notes_less_oct_arr([_]).
notes_less_oct_arr([N1, N2 | T]) :- less_then_oct(N1, N2), notes_less_oct_arr([N2 | T]).

% по ноте и другой ноте, у которой не задана октава, подбирает октаву так, чтобы он была ближе всего
% предполагается, что нота2 лежит ниже ноты1 в рамках одного аккорда
nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)) :-  Octave2 #= Octave1 - 1, Stage1 #< Stage2.
nearest_down(note(Octave1, Stage), note(Octave2, Stage)) :-  Octave2 #= Octave1 - 1.
nearest_down(note(Octave, Stage1), note(Octave, Stage2)) :-  Stage1 #> Stage2.

% поиск ближайшей нижней ноты с учетом возможности ухода баса вниз на 2 октавы
nearest_down_bass(Bass, Bass).

nearest_down_bass(note(Octave1, Stage1), note(Octave2, Stage2)) :-
    nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)).

nearest_down_bass(note(Octave1, Stage1), note(Octave2_down, Stage2)) :-
    nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)),
    Octave2_down #= Octave2 - 1.

stage_pitch(0, 0).
stage_pitch(1, 2).
stage_pitch(2, 4).
stage_pitch(3, 5).
stage_pitch(4, 7).
stage_pitch(5, 9).
stage_pitch(6, 11).

abs_pitch(pitch(Octave, Stage, Alter), Pitch) :- stage_pitch(Stage, StagePitch), Pitch #= Octave * 12 + StagePitch + Alter.

note_sub(pitch(Octave1, Stage1, Alter1), pitch(Octave2, Stage2, Alter2), interval(Stage, Semitones)) :-
   abs_pitch(pitch(Octave1, Stage1, Alter1), Pitch1),
   abs_pitch(pitch(Octave2, Stage2, Alter2), Pitch2),
   Stage #= Octave1 * 7 + Stage1 - Octave2 * 7 - Stage2,
   Semitones #= Pitch1 - Pitch2.

add_interval(Note1, Interval, Note2) :- note_sub(Note2, Note1, Interval).

interval_less(interval(_, Semi1), interval(_, Semi2)) :- Semi1 #< Semi2.
interval_le(X, X).
interval_le(X, Y) :- interval_less(X, Y).

nearests_down([], []).
nearests_down([NoteA|ATail], [NoteB|BTail]) :- nearest_down(NoteA, NoteB), nearests_down(ATail, BTail).

nearests_down_bass([], []).
nearests_down_bass([NoteA|ATail], [NoteB|BTail]) :- nearest_down_bass(NoteA, NoteB), nearests_down_bass(ATail, BTail).

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

% разрешенные названия аккордов
chord_types([ta, da, sa]).
% По типу аккорда возвращает список ступеней аккорда: прима, терция, квинта
% ta - тоника
% da - доминанта
% sa - субдоминанта
chord_stages(ta, [1, 3, 5]).
chord_stages(da, [5, 7, 2]).
chord_stages(sa, [4, 6, 1]).

% ступень содержится в аккорде
is_in_chord(N, Chord) :- chord_stages(Chord, X), member(N, X).

% Прима аккорда
chord_primo(X, Chord) :- chord_stages(Chord, [X | _]).

% соседние ноты в аккорде
% UpperStage: верхняя нота
% ChordType: {ta, da, sa}
% LowerStage: нижняя нота
% wide/narrow - широкое\тесное расположение
chord_neighbours(UpperStage, ChordType, LowerStage, wide) :- chord_stages(ChordType, ChordStages), rnext(UpperStage, ChordStages, LowerStage).
chord_neighbours(UpperStage, ChordType, LowerStage, narrow) :- chord_stages(ChordType, ChordStages), rnext(LowerStage, ChordStages, UpperStage).

% Гармонизация 4-х ступеней по одной известной ступени
% Stage1: int \in [1, 7] - верхняя нота (номер ступени)
% ChordType: {ta, da, sa} - тип аккорда
% Stage2: int \in [1, 7]
% Stage3: int \in [1, 7]
% Stage4: int \in [1, 7] - бас
% ChordArrangement: {wide, narrow}
harm1(Stage1, ChordType, Stage2, Stage3, Stage4, ChordArrangement) :-
    chord_types(AllowedChordTypes),
    member(ChordType, AllowedChordTypes),
    member(ChordArrangement, [wide, narrow]),
    % поиск типа аккорда по заданной ступени
    is_in_chord(Stage1, ChordType),
    chord_primo(Stage4, ChordType),
    % вычисляем следующие ступени
    chord_neighbours(Stage1, ChordType, Stage2, ChordArrangement),
    chord_neighbours(Stage2, ChordType, Stage3, ChordArrangement).


% разрешенные последовательности аккордов
% После T и S может быть всё, что угодно.
possible_next_chord(X, Y) :- member(X, [ta, sa]),
                             chord_types(AllowedChordTypes),
                             member(Y, AllowedChordTypes).

% запрещен ход D - S
possible_next_chord(da, Y) :- member(Y, [da, ta]).

% Гармонизация списков нот, аккордов и расположений без учета октав.
% По заданному списку нот (как правило, это будет либо [N1] - сопрано,
% либо - [N4] - бас) возвращает списки нот оставшихся 3-х голосов,
% типы аккордов, и их расположение.
harm_stages([N1], [TDS], [N2], [N3], [N4], [W]) :-
   harm1(N1, TDS, N2, N3, N4, W).

harm_stages([N1, NN1 | NS1], [TDS, TDSN | TDSS], [N2, NN2 | NS2], [N3, NN3 | NS3], [N4, NN4 | NS4], [W, WN | WS]) :-
    % поиск первого аккорда
    harm1(N1, TDS, N2, N3, N4, W),
    possible_next_chord(TDS, TDSN),
    harm_stages([NN1 | NS1], [TDSN | TDSS], [NN2 | NS2], [NN3 | NS3], [NN4 | NS4], [WN | WS]).

stages([], []).
stages([note(_, N) | T], [N | TS]) :- stages(T, TS).

% Нам даны 2 массива с нотами, соответствующие 2-м соседним аккордам.
% Каждый массив содержит 4 ноты, соответствующие 4-м голосам от
% верхнего к нижнему.
% Предикат проверяет, что не все голоса идут в одну сторону и не все одинаковы.
%% not_all([X | T]) :- \+ maplist(==(X), T).
%% not_all([X1, X2]) :- X1 #\= X2.
not_all([X1, X2 | T]) :- X1 #\= X2.
not_all([X, X | T]) :- not_all([X | T]).
not_all_one([N1 | T1], [N2 | T2]) :- maplist(notes_cmp, [N1 | T1], [N2 | T2], Deltas),
                                     not_all(Deltas).


% Аккорды не равны
tne(da, ta).
tne(da, sa).
tne(sa, ta).
tne(sa, da).
tne(ta, da).
tne(ta, sa).

% прямое перемещение в рамках одного аккорда.
% Сохраняются тип аккорда, расположение и бас.
one_chord(note(Octave1, Stage1), note(Octave2, Stage2), note(Octave3, Stage3), note(Octave4, Stage4), ChordType, ChordArrangement, note(Octave1N, Stage1N), note(Octave1N, Stage1N), note(Octave1N, Stage1N), note(Octave4, Stage4)) :-
    harm1(Stage1, ChordType, Stage2, Stage3, Stage4, ChordArrangement),
    nearest_down(note(Octave1, Stage1), note(Octave2, Stage2)),
    nearest_down(note(Octave2, Stage2), note(Octave3, Stage3)),
    nearest_down_bass(note(Octave3, Stage4), note(Octave4, Stage4)).
    %% harm1(Stage1N, ChordType, Stage2N, Stage3N, Stage4, ChordArrangement),


wnswitch([_], [_]).
wnswitch([A, A | TS], [wide, narrow | WS]) :- wnswitch([A | TS], [narrow | WS]).
wnswitch([A, A | TS], [narrow, wide | WS]) :- wnswitch([A | TS], [wide | WS]).
wnswitch([_, B | TS], [W, W | WS]) :- wnswitch([B | TS], [W | WS]).

wnswitch_n1([_], [_]).
wnswitch_n1([_, N2 | T], [W, W | WS]) :- wnswitch_n1([N2 | T], [W | WS]).
wnswitch_n1([N1, N2 | T], [narrow, wide | WS]) :- stage_less(N1, N2),
                                                 wnswitch_n1([N2 | T], [wide | WS]).
wnswitch_n1([N1, N2 | T], [wide, narrow | WS]) :- stage_less(N2, N1),
                                                 wnswitch_n1([N2 | T], [narrow | WS]).


same_length([], []).
same_length([_|A], [_|B]) :- same_length(A,B).

dirs([_], [_], [_], [_]).
dirs([A1, A2 | AS], [B1, B2 | BS], [C1, C2 | CS], [D1, D2 | DS]) :-
    not_all_one([A1, B1, C1, D1], [A2, B2, C2, D2]),
    dirs([A2 | AS], [B2 | BS], [C2 | CS], [D2 | DS]).

check_downbeat_step([_], [_], _).

check_downbeat_step([ChordType1, ChordType2 | ChordTypes], [W1, W2 | Ws], MaxW) :-
    tne(ChordType1, ChordType2), check_downbeat_step([ChordType2 | ChordTypes], [W2 | Ws], W2).

check_downbeat_step([ChordType, ChordType | ChordTypes], [W1, W2 | Ws], MaxW) :-
    W2 #< MaxW, check_downbeat_step([ChordType | ChordTypes], [W2 | Ws], MaxW).

check_downbeat(X, [W | Ws]) :- check_downbeat_step(X, [W | Ws], W).

check_measures([_], [_]).

check_measures([ChordType1, ChordType2 | ChordTypes], [_, start | Ss]) :-
    tne(ChordType1, ChordType2), check_measures([ChordType2 | ChordTypes], [start | Ss]).

check_measures([_, X | ChordTypes], [_, non_start | Ss]) :-
    check_measures([X | ChordTypes], [non_start | Ss]).

% Гармонизация списков нот в формате note(Октава, Ступень)
harm(N1, TDS, N2, N3, N4, W) :-
   same_length(N1, TDS),
   same_length(N1, N2),
   same_length(N1, N3),
   same_length(N1, N4),
   same_length(N1, W),
   % выбираем ноту из пары (октава, нота), игнорируя октаву
   stages(N1, NN1),
   stages(N2, NN2),
   stages(N3, NN3),
   stages(N4, NN4),
   % без учета октавы
   harm_stages(NN1, TDS, NN2, NN3, NN4, W),
   wnswitch(TDS, W),
   wnswitch_n1(N1, W),
   nearests_down(N1, N2),
   nearests_down(N2, N3),
   nearests_down_bass(N3, N4),
   notes_less_oct_arr(N4),
   dirs(N1, N2, N3, N4).


% чтение файла

getAlter(MNote, Alter) :-
       member(element(alter, _, [AlterChar]), MNote), atom_number(AlterChar, Alter), !.
getAlter(_, 0).

evalExpr(X, NX) :- NX #= X.

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
       getAlter(P, Alter).

appendNote(Elem, Ts, Voice, Duration, Tail, DstList) :-
       member(element(pitch, _, P), Elem),
       readPitch(P, Octave, Step, Alter),
       append([xml_note(Ts, voice(Voice), duration(Duration), pitch(Octave, Step, Alter))], Tail, DstList ).

appendNote(_, _, _, _, Tail, DstList) :-
       DstList = Tail.

getNote(MNote, xnote(Octave, StepChar, Alter)) :-
       member(element(step, _, [StepChar]), MNote),
       member(element(octave, _, [OctaveChar]), MNote),
       atom_number(OctaveChar, Octave),
       getAlter(MNote, Alter).

getNotes([], _, _, [], _).

getNotes([element(note, _, N) | Tail], MeasureNumber, Mult, Notes, Time) :-
       getNoteAttrs(N, Voice, Duration),
       getNotes(Tail, MeasureNumber, Mult, MT, Time + Duration * Mult),
       NTime #= Time,
       NDur #= Duration * Mult,
       appendNote(N, ts(MeasureNumber,NTime), Voice, NDur, MT, Notes),!.

getNotes([element(backup, _, B) | Tail], MeasureNumber, Mult, Notes, Time) :-
       member(element(duration, _, [Dur]), B),
       atom_number(Dur, Backup),
       getNotes(Tail, MeasureNumber, Mult, Notes, Time - Backup * Mult), !.

getNotes([_ | Tail], MeasureNumber, Mult, Notes, Time) :-
       getNotes(Tail, MeasureNumber, Mult, Notes, Time).

getNotesFromMeasure(M, MeasureNumber, DurationMult, Notes) :-
       getNotes(M, MeasureNumber, DurationMult, Notes, 0).

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

% [Chord(Pitches, Ts, Dur), ChordsTail]
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

% TODO: check individual chord
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

harm2(music_attrs(Key, Beats), Chords) :-
   checkIndividualChords(Key, Chords),
   checkChordPairs(music_attrs(Key, Beats), Chords),
   checkVoicePairChords(0, 1, Chords),
   checkVoicePairChords(0, 2, Chords),
   checkVoicePairChords(0, 3, Chords),
   checkVoicePairChords(1, 2, Chords),
   checkVoicePairChords(1, 3, Chords),
   checkVoicePairChords(2, 3, Chords).

% getTimes(element(part, _, [element(measure, _, M) | Tail]), Offset, Times) :-
%       .

% [elem(ts(Measure, Time), Dur, Acc, [Oct, Stage, Alt])]

getNotes([], []).
getNotes([element(note, _, [element(pitch, _, MNote) | _])|Tail], [XNote|OTail]) :-
       getNote(MNote, XNote),
       getNotes(Tail, OTail), !.
getNotes([_|Tail], OTail) :- getNotes(Tail, OTail).

getElements(_, [], []).
getElements(E, [element(E, _, M) | T], MO) :- getElements(E, T, MT), append(M, MT, MO), !.
getElements(E, [_|T], MO) :- getElements(E, T, MO).

shift('C', 0).
shift('D', 2).
shift('E', 4).
shift('F', 5).
shift('G', 7).
shift('A', 9).
shift('B', 11).

altitude(xnote(O, N, A), H) :- shift(N, D), H #= (O * 12) + D + A.
altitudes([], []).
altitudes([A|AS], [O|OS]) :- altitude(A, O), altitudes(AS, OS).

altitude2note(S, T, A, note(O, N)) :-
	X #= mod(A - S, 12),
	member(stage(X, N), T),
	O #= div(A - S, 12).
altitudes2notes(_, _, [], []).
altitudes2notes(S, T, [A|AS], [N|NS]) :- altitude2note(S, T, A, N), altitudes2notes(S, T, AS, NS).

% соответствие между номером мажорной ступени в хроматической гамме
% и номером ступени в мажорной тональности
tons(maj, [stage(0,1), stage(2,2), stage(4, 3), stage(5, 4), stage(7, 5), stage(9, 6), stage(11, 7)]).

readMXML(File, XNotes, Alts) :-
       musicxml_score(File, element(_, _, S)),
       getElements(part, S, P),
       getElements(measure, P, M),
       getNotes(M, XNotes),
       altitudes(XNotes, Alts).

readNotes(File, Shift, Scale, Notes) :-
       readMXML(File, _, Alts),
       tons(Scale, List),
       altitudes2notes(Shift, List, Alts, Notes).

harmFile(File, S, T, N1, N2, N3, N4, C, W) :-
     readNotes(File, S, T, N1),
     harm(N1, C, N2, N3, N4, W).
