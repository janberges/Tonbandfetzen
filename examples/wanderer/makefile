.PHONY: all clean

all: wanderer.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $< $@

parts/verse.wav: chords/a.wav chords/b.wav chords/b.wav chords/a.wav
	stick $+ $@

parts/chorus.wav: chords/c.wav melody/melody.wav
	stack $^ $@

wanderer.wav: chords/e.wav parts/verse.wav parts/chorus.wav chords/d.wav chords/e.wav
	stick $+ $@
