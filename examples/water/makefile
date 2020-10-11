.PHONY: all clean

all: water.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $<

parts/fsh.wav: bass/fsh.wav melody/verse.wav
	stack $^ $@

parts/b.wav: bass/b.wav melody/verse.wav
	stack $^ $@

parts/csh.wav: bass/csh.wav melody/segue.wav
	stack $^ $@

parts/chords.wav: chords/fsh.wav chords/b.wav chords/csh.wav
	stick $^ $@

parts/melody.wav: parts/fsh.wav parts/fsh.wav parts/fsh.wav parts/fsh.wav parts/b.wav parts/b.wav parts/csh.wav
	stick $+ $@

pattern/patternx8.wav: pattern/pattern.wav
	repeat $< 8 $@

parts/pattern.wav: pattern/break.wav pattern/patternx8.wav
	stick $^ $@

water.wav: parts/melody.wav parts/chords.wav parts/pattern.wav
	stack $^ $@
