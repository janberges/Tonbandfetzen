.PHONY: wav mp3 clean

wav: wanderer.wav
mp3: wanderer.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tz tag $<
	ffmpeg -y -i $< $@

%.wav: %.mel
	tz mel $< $@

parts/verse.wav: chords/a.wav chords/b.wav chords/b.wav chords/a.wav
	tz stick $+ $@

parts/chorus.wav: chords/c.wav melody/melody.wav
	tz stack $^ $@

wanderer.wav: chords/e.wav parts/verse.wav parts/chorus.wav chords/d.wav chords/e.wav
	tz stick $+ $@
