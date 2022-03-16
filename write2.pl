:- [utility].

:- use_module(pitch_arithm2).
:- use_module(library(clpfd)).
:- use_module(library(sgml_write)).

fifthMatch(0, 0).
fifthMatch(1, 2).
fifthMatch(2, 4).
fifthMatch(3, -1).
fifthMatch(4, 1).
fifthMatch(5, 3).
fifthMatch(6, 5).

convMaj(maj, Note, Alt, Note, Alt).
convMaj(min, Note, Alt, Note2, Alt2) :-
	add_interval(pitch(0, Note, Alt), interval(2,3), pitch(_, Note2, Alt2)).

convKeyToFifth(key(Note, Alt, Scale), Fifth) :-
	convMaj(Scale, Note, Alt, NoteMaj, AltMaj),
	fifthMatch(NoteMaj,FifthsBase),
	Fifth #= FifthsBase + AltMaj * 7.

getFirstMeasureAttrs(Element, music_attrs(Key, beats(Nom, Den))) :-
	convKeyToFifth(Key, Fifth),
	NormedDen #= Den div 180,
	number_string(Fifth, SFifth),
	number_string(Nom, SNom),
	number_string(NormedDen, SDen),
	Element = element("attributes", [], [
		element("divisions", [], ["180"]),
		element("key", [], [
			element("fifths", [], [SFifth])
		]),
		element("time", [], [
			element("beats", [], [SNom]),
			element("beat-type", [], [SDen])
		]),
		element("staves", [], ["2"]),
		element("clef", [number="1"], [
			element("sign", [], ["G"]),
			element("line", [], ["2"])
		]),
		element("clef", [number="2"], [
			element("sign", [], ["F"]),
			element("line", [], ["4"])
		])
	]).

stageNum('C', 0).
stageNum('D', 1).
stageNum('E', 2).
stageNum('F', 3).
stageNum('G', 4).
stageNum('A', 5).
stageNum('B', 6).

voiceStaff(0, 1, "1").
voiceStaff(1, 1, "2").
voiceStaff(2, 2, "1").
voiceStaff(3, 2, "2").

getNoteElement(Element, Voice, stage(Oct, Step, Alter), Dur) :-
	voiceStaff(Voice, Staff, SVoice),
	number_string(Oct, SOct),
	number_string(Alter, SAlter),
	number_string(Dur, SDur),
	stageNum(SStep, Step),
	Element = element("note", [], [
		element("pitch", [], [
			element("step", [], [SStep]),
			element("alter", [], [SAlter]),
			element("octave", [], [SOct])
		]),
		element("duration", [], [SDur]),
		element("voice", [], [SVoice]),
		element("staff", [], [Staff])
	]).

getBackupElement(Element, Dur) :-
	number_string(Dur, SDur),
	Element = element("backup", [], [
		element("duration", [], [SDur])
	]).

getRestElement(Element, Dur) :-
	number_string(Dur, SDur),
	Element = element("rest", [], [
		element("duration", [], [SDur])
	]).

firstBackup(Element, Time) :- 
	Time > 0, 
	getRestElement(NElement, Time), Element=opt(NElement).

firstBackup(Element, Time) :- 
	Time < 0, MTime #= -Time, 
	getBackupElement(NElement, MTime), Element=opt(NElement).

firstBackup(nullopt, _).

optFirst(List, nullopt, List).
optFirst([E|List], opt(E), List).

getMeasureChordElements(Elements, [], _) :- Elements=[].

getMeasureChordElements(Elements, 
	[ chord([N1, N2, N3, N4], ts(_, Ts), duration(Dur), _)
		| Chords], CurTs) :-
	ConvDur #= Dur,
	TimeShift #= Ts - CurTs,
	firstBackup(Beg, TimeShift),
	getNoteElement(E1, 0, N1, ConvDur),
	getNoteElement(E2, 1, N2, ConvDur),
	getNoteElement(E3, 2, N3, ConvDur),
	getNoteElement(E4, 3, N4, ConvDur),
	getBackupElement(B, ConvDur),
	NextTs #= CurTs + Dur,
	getMeasureChordElements(NextElements, Chords, NextTs),
	NElements = [E1, B, E2, B, E3, B, E4 | NextElements],
	optFirst(Elements, Beg, NElements).

getMXMLbaseElements(Elements, Measures) :- 
	Elements = [element("score-partwise", [version="3.1"], [
		element("part-list", [], [
			element("score-part", [id="P1"], [
				element("part-name", [], ["Piano"]),
				element("score-instrument", [id="P1-I1"], [
			        element("instrument-name",[],["Piano"])
				])
			])
		]),
		element("part", [id="P1"], Measures)
	])].

writeMXML(File, Elements) :-
	open(File,write,Stream),
	write(Stream,'<?xml version="1.0" encoding="UTF-8"?>\n<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 3.1 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd">\n'),
	xml_write(Stream, Elements, [header(false)]),
	close(Stream).

splitoffNextMeasure([chord(Notes, ts(Num, Ts), D, A)| Chords], ChordRest, Measure, Num) :-
	splitoffNextMeasure(Chords, ChordRest, MeasureRest, Num),
	Measure = [chord(Notes, ts(Num, Ts), D, A) | MeasureRest].

% if empty ChordList, or Num of next chord doesn't match Num of measure
splitoffNextMeasure(ChordList, ChordList, [], _).


convertChordListToMeasureList([chord(Notes, ts(Num, Ts), D, A)| Chords], [measure(Num, MeasureChords)| Measures]) :-
	splitoffNextMeasure([chord(Notes, ts(Num, Ts), D, A)| Chords], ChordsRest, MeasureChords, Num),
	convertChordListToMeasureList(ChordsRest, Measures).

convertChordListToMeasureList([],[]).

getMeasureElement(Element, measure(Num, Chords)) :- 
	number_string(Num,SNum),
	getMeasureChordElements(Elements, Chords, 0),
	Element=element("measure", [number=SNum], Elements).

getMeasureElement(Element, measure(Num, Chords), Attrs) :- 
	number_string(Num,SNum),
	getMeasureChordElements(Elements, Chords, 0),
	Element=element("measure", [number=SNum], [Attrs|Elements]).

convertMeasuresToElementsNoAttrs([Element0 | Elements], [Measure | Measures]) :-
	getMeasureElement(Element0, Measure),
	convertMeasuresToElementsNoAttrs(Elements, Measures).

convertMeasuresToElementsNoAttrs([], []).

convertMeasuresToElements([Element0 | Elements], [Measure | Measures], Attrs) :-
	getMeasureElement(Element0, Measure, Attrs),
	convertMeasuresToElementsNoAttrs(Elements, Measures).

testWrite(File) :- 
	getMeasureChordElements(Notes, [
	chord([
		stage(5,4,-1),
		stage(4,3,0),
		stage(3,2,1),
		stage(2,1,0)
	], ts(0,0), duration(360), 0),
	chord([
		stage(5,5,-1),
		stage(4,4,0),
		stage(3,3,1),
		stage(2,2,0)
	], ts(0,360), duration(360), 0)], 0),
	getFirstMeasureAttrs(Attrs,  music_attrs(key(0, 0, maj), beats(4, 720))),
	getMXMLbaseElements(XML, [element("measure", [number="1"], [Attrs|Notes])]),
	writeMXML(File, XML).

writeChordsToFile(File, Chords, MusicAttrs) :-
	getFirstMeasureAttrs(Attrs,  MusicAttrs),
	convertChordListToMeasureList(Chords, Measures),
	convertMeasuresToElements(Elements, Measures, Attrs),
	getMXMLbaseElements(XML, Elements),
	writeMXML(File, XML).


writeChordsToFile(File) :-
	writeChordsToFile(File, [
	chord([
		stage(5,4,-1),
		stage(4,3,0),
		stage(3,2,1),
		stage(2,1,0)
	], ts(0,0), duration(360), 0),
	chord([
		stage(5,5,-1),
		stage(4,4,0),
		stage(3,3,1),
		stage(2,2,0)
	], ts(0,360), duration(360), 0),chord([
		stage(5,4,-1),
		stage(4,3,0),
		stage(3,2,1),
		stage(2,1,0)
	], ts(1,0), duration(360), 0),
	chord([
		stage(5,5,-1),
		stage(4,4,0),
		stage(3,3,1),
		stage(2,2,0)
	], ts(1,360), duration(360), 0)], 
	music_attrs(key(2, -1, maj), beats(4, 720))).
