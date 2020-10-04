.PHONY: all clean

all: water.aif

clean:
	git clean -Xdf

%.aif: %.mel
	mel $<

parts/fsh.aif: bass/fsh.aif melody/verse.aif
	stack $^ $@

parts/b.aif: bass/b.aif melody/verse.aif
	stack $^ $@

parts/csh.aif: bass/csh.aif melody/segue.aif
	stack $^ $@

parts/chords.aif: chords/fsh.aif chords/b.aif chords/csh.aif
	stick $^ $@

parts/melody.aif: parts/fsh.aif parts/fsh.aif parts/fsh.aif parts/fsh.aif parts/b.aif parts/b.aif parts/csh.aif
	stick $+ $@

pattern/patternx8.aif: pattern/pattern.aif
	repeat $< 8 $@

parts/pattern.aif: pattern/break.aif pattern/patternx8.aif
	stick $^ $@

water.aif: parts/melody.aif parts/chords.aif parts/pattern.aif
	stack $^ $@
